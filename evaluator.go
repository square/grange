package grange

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"sync"

	"time"

	"github.com/orcaman/concurrent-map"
	"gopkg.in/deckarep/v1/golang-set"
)

// State holds data that queries operate over. Queries in grange are
// deterministic, so the same query will always return the same result for a
// given state. Clients are expected to build their own state to query from
// their own datasource, such as a database or files on disk.
//
// State maintains an internal cache of expanded values to speed up queries.
// After constructing a large state it is recommended to call PrimeCache()
// before querying, otherwise initial queries will likely take longer than
// later ones as the cache is built up incrementally.
type State struct {
	clusters       map[string]Cluster
	defaultCluster string

	// Populated lazily as groups are evaluated. They won't change unless state
	// changes.
	// clusterCache map[string]map[string]*Result
	// To make it concurrent read write capable , using concurrent-map.
	// As concurrent-map stores value type as interface,
	// we need to cast them back whenever we retrive values
	// cmap.ConcurrentMap is of type map[string] interface
	clusterCache cmap.ConcurrentMap

	// clusters() query is expensive, we want to cache results of each such query
	// var cachedClusterQueryResults map[string] Result
	cachedCQR cmap.ConcurrentMap
	metrics   metrics
}

type metrics struct {
	// useful to know how old state is
	initializedAt time.Time

	cacheInitializedAt     time.Time
	errorsDuringCacheBuild int

	buildTimeForCache        float64
	buildTimeForClusterCache float64
	buildTimeForCachedCQR    float64

	numberOfTruncatedResults int64
}

// A Cluster is mapping of arbitrary keys to arrays of values. The only
// required key is CLUSTER, which is the default set of values for the cluster.
type Cluster map[string][]string

// A set of values returned by a query. The size of this set is limited by
// MaxResults.
type Result struct {
	mapset.Set
}

var (
	// Maximum number of characters that grange will try to parse in a query.
	// Queries longer than this will be rejected. This limit also applies to
	// cluster and group names and values. Combined with MaxResults, this limits
	// result sizes to approximately 4MB.
	MaxQuerySize = 4000

	// The maximum number of results a query can return. Execution will be
	// short-circuited once this many results have been gathered. No error will
	// be returned.
	MaxResults = 10000

	// Maximum number of subqueries that will be evaluated, including evaluation
	// of cluster values. If this is exceeded, an error will be returned.
	// Primarily useful for aborting cycles, but also can shortcut really
	// expensive queries. This should not be exceeded in normal operation.
	MaxQueryDepth = 100

	// The default cluster for new states, used by @ and ? syntax. Can be changed
	// per-state using SetDefaultCluster.
	DefaultCluster = "GROUPS"

	// Parallelism to be used while building primeCache
	primeCacheParallelismFactor = 2
)

// Clusters is a getter for all clusters that have been added to the state.
// There isn't really a good reason to use this other than for debugging
// purposes.
func (s *State) Clusters() map[string]Cluster {
	return s.clusters
}

// NewState creates a new state to be passed into EvalRange. This will need to
// be used at least once before you can query anything.

func NewState() State {
	state := State{
		clusters:       map[string]Cluster{},
		defaultCluster: DefaultCluster,
		metrics:        metrics{},
	}
	state.metrics.initializedAt = time.Now()
	state.ResetCache()
	return state
}

// NewResult is mostly used internally, but is handy in testing scenarios when
// you need to compare a query result to a known value.
func NewResult(args ...interface{}) Result {
	return Result{mapset.NewSetFromSlice(args)}
}

// AddCluster adds a new cluster to the state and resets the cache.
func (state *State) AddCluster(name string, c Cluster) {
	state.clusters[name] = c
	state.ResetCache()
}

// Changes the default cluster for the state.
func (state *State) SetDefaultCluster(name string) {
	state.defaultCluster = name
}

// PrimeCache traverses over the entire state to expand all values and store
// them in the state's cache. Subsequent queries will be able to use the cache
// immediately, rather than having to build it up incrementally.
//
// It returns all errors encountered during the traverse. This isn't
// necessarily a critical problem, often errors will be in obscure keys, but
// you should probably try to fix them.
func (state *State) PrimeCache() []error {
	// Limiting parallelism to 2
	// splitting clusters in slices and spawn go routine for each
	startTime := time.Now()
	clusters := state.clusterNamesAsArray()
	arrayOfClusterSlices := splitIntoSlices(clusters, primeCacheParallelismFactor)
	var wg sync.WaitGroup
	resultCh := make(chan mapset.Set, primeCacheParallelismFactor)
	errorCh := make(chan []error, primeCacheParallelismFactor)
	defer close(resultCh)
	defer close(errorCh)
	for _, slice := range arrayOfClusterSlices {
		wg.Add(1)
		go func(s []string) {
			defer wg.Done()
			res, err := buildPrimeClusterCacheForSlice(state, s)
			resultCh <- res
			errorCh <- err
		}(slice)
	}
	done := make(chan interface{})
	go func() {
		wg.Wait()
		// sleep makes thats buffers in resultCh and errorCh are read
		// time.Sleep(1*time.Millisecond)
		close(done)
	}()
	results := mapset.NewSet()
	errors := []error{}

Loop:
	for {
		select {
		case r := <-resultCh:
			results = results.Union(r)
		case err := <-errorCh:
			errors = append(errors, err...)
		case <-done:
			if len(resultCh) == 0 && len(errorCh) == 0 {
				break Loop
			}
		}
	}
	// end of Loop:

	wg.Wait()
	state.metrics.buildTimeForClusterCache = time.Since(startTime).Seconds()
	state.populateCachedCQRforSet(results)
	state.metrics.buildTimeForCachedCQR =
		time.Since(startTime).Seconds() - state.metrics.buildTimeForClusterCache
	state.metrics.buildTimeForCache = time.Since(startTime).Seconds()
	state.metrics.errorsDuringCacheBuild = len(errors)
	state.metrics.cacheInitializedAt = time.Now()
	return errors
}

// StateMetrics returns metrics related to state, cache as map[string]int64
func (state *State) StateMetrics() map[string]int64 {
	metrics := make(map[string]int64)
	metrics["stateInitializedAt"] = state.metrics.initializedAt.Unix()
	metrics["cacheInitializedAt"] = state.metrics.cacheInitializedAt.Unix()
	metrics["numberOfClusters"] = int64(len(state.clusters))
	metrics["numberOfCachedClusters"] = int64(state.clusterCache.Count())
	metrics["numberOfcachedCQR"] = int64(state.cachedCQR.Count())
	metrics["cacheTotalBuildTimeInSeconds"] = int64(state.metrics.buildTimeForCache)
	metrics["cacheBuildTimeForClustersInSeconds"] = int64(state.metrics.buildTimeForClusterCache)
	metrics["cacheBuildTimeForCQRInSeconds"] = int64(state.metrics.buildTimeForCachedCQR)
	metrics["errorsDuringCacheBuild"] = int64(state.metrics.errorsDuringCacheBuild)
	metrics["numberOfTruncatedResults"] = state.metrics.numberOfTruncatedResults
	return metrics
}

// buildPrimeClusterCacheForSlice is used internally for building ClusterCache.
// returns results of CLUSTER_NAME:CLUSTER , which is used for building cachedCQR
// also returns array of errors encounter during parsing clusters
func buildPrimeClusterCacheForSlice(state *State, clusters []string) (mapset.Set, []error) {
	var errs []error
	res := mapset.NewSet()
	for _, clusterName := range clusters {
		context := newContext()
		context.currentClusterName = clusterName
		for key, _ := range state.clusters[clusterName] {
			err := clusterLookup(state, &context, key)
			// we are interested only results for key CLUSTER
			if key == "CLUSTER" {
				res = res.Union(context.currentResult.Set)
			}
			if err != nil {
				errs = append(errs, err)
			}
		}
	}
	return res, errs
}

func (state *State) clusterNamesAsArray() []string {
	i := 0
	ret := make([]string, len(state.clusters))
	for name := range state.clusters {
		ret[i] = name
		i++
	}
	return ret
}

// splices array into ~'count' of slices
// if len(array) is less than count,
// we splice array into slices of length 1
// if len(array) is not multiple of count,
// we end up returning more than 'count' number of slices
func splitIntoSlices(array []string, count int) [][]string {
	var ret [][]string
	lengthOfArray := len(array)
	if lengthOfArray == 0 {
		return append(ret, array)
	}
	sliceLength := lengthOfArray / count
	if sliceLength == 0 {
		sliceLength = 1
	}
	for i := 0; i < lengthOfArray; i += sliceLength {
		if i+sliceLength < lengthOfArray {
			ret = append(ret, array[i:i+sliceLength])
		} else {
			ret = append(ret, array[i:lengthOfArray])
		}
	}
	return ret
}

// ResetCache clears cached expansions. The public API for modifying state
// already calls this when necessary, so you shouldn't really have a need to
// call this.
func (state *State) ResetCache() {
	// state.clusterCache = map[string]map[string]*Result{}
	state.clusterCache = cmap.New()
	state.cachedCQR = cmap.New()
}

// Query is the main interface to grange. See the main package documentation
// for query language specification. On error, an empty result is returned
// alongside the error. Queries that are longer than MaxQuerySize are
// considered errors.
//
// The size of the returned result is capped by MaxResults.
//
// This method is only thread-safe if PrimeCache() has previously been called
// on the state.
func (state *State) Query(input string) (Result, error) {
	if len(input) > MaxQuerySize {
		return NewResult(),
			errors.New(fmt.Sprintf("Query is too long, max length is %d", MaxQuerySize))
	}

	context := newContext()
	return evalRangeWithContext(input, state, &context)
}

// Used for populating state.cachedCQR
// Go over all clusters in state.clusters, look if element is part of cluster definition,
// if yes, add name of cluster to cachedCQR[element]
// parse through all clusters before adding to state.cachedCQR
// as a precaution, we do not want to cache empty results
func (state *State) populateCachedCQRforSet(set mapset.Set) {
	context := newContext()
	// skip processing for already cached elements
	notCached := make(map[string]Result)
	for element := range set.Iter() {
		if !state.cachedCQR.Has(element.(string)) {
			notCached[element.(string)] = NewResult()
		}
	}

	if len(notCached) == 0 {
		return
	}

	for clusterName, _ := range state.clusters {
		subContext := context.subCluster(clusterName)
		clusterLookup(state, &subContext, "CLUSTER")
		for key := range notCached {
			if subContext.currentResult.Contains(key) {
				notCached[key].Add(clusterName)
			}
		}
	}
	for key, val := range notCached {
		if val.Cardinality() > 0 {
			state.cachedCQR.Set(key, val)
		}
	}
}

func (state *State) addValueToCachedCQR(key string, value string) {
	if tmp, ok := state.cachedCQR.Get(key); ok {
		tmp.(Result).Add(value)
	} else {
		state.cachedCQR.Set(key, NewResult(value))
	}
}

type tooManyResults struct{}

type evalContext struct {
	currentClusterName string
	currentResult      Result
	workingResult      *Result
	depth              int
}

func newContext() evalContext {
	return evalContext{currentResult: NewResult()}
}

func parseRange(input string) (parserNode, error) {
	r := &rangeQuery{Buffer: input}
	r.Init()
	if err := r.Parse(); err != nil {
		return nil, err
	}
	r.Execute()
	if len(r.nodeStack) > 0 {
		return r.nodeStack[0], nil
	} else {
		return nodeNull{}, nil
	}
}

func evalRangeWithContext(input string, state *State, context *evalContext) (Result, error) {
	err := evalRangeInplace(input, state, context)

	return context.currentResult, err
}

// Useful internally so that results do not need to be copied all over the place
func evalRangeInplace(input string, state *State, context *evalContext) (err error) {
	if context.depth > MaxQueryDepth {
		return errors.New("Query exceeded maximum recursion limit")
	}
	node, parseError := parseRange(input)
	if parseError != nil {
		return errors.New("Could not parse query: " + input)
	}

	defer func() {
		if r := recover(); r != nil {
			switch r.(type) {
			case tooManyResults:
				// No error returned, we just chop off the results
				state.metrics.numberOfTruncatedResults += 1
				err = nil
			case error:
				err = r.(error)
			default:
				panic(r)
			}
		}
	}()

	return node.(evalNode).visit(state, context)
}

func (c evalContext) hasResults() bool {
	return c.currentResult.Cardinality() == 0
}

func (n nodeBraces) visit(state *State, context *evalContext) error {
	leftContext := context.sub()
	rightContext := context.sub()
	middleContext := context.sub()

	if err := n.left.(evalNode).visit(state, &leftContext); err != nil {
		return err
	}
	if err := n.node.(evalNode).visit(state, &middleContext); err != nil {
		return err
	}
	if err := n.right.(evalNode).visit(state, &rightContext); err != nil {
		return err
	}

	if leftContext.hasResults() {
		leftContext.addResult("")
	}
	if middleContext.hasResults() {
		middleContext.addResult("")
	}
	if rightContext.hasResults() {
		rightContext.addResult("")
	}

	for l := range leftContext.resultIter() {
		for m := range middleContext.resultIter() {
			for r := range rightContext.resultIter() {
				context.addResult(fmt.Sprintf("%s%s%s", l, m, r))
			}
		}
	}

	return nil
}

// Hack, see note on nodeBraceStart definition in nodes.go
func (n nodeBraceStart) visit(state *State, context *evalContext) error {
	return nil
}

func (n nodeLocalClusterLookup) visit(state *State, context *evalContext) error {
	var evalErr error

	subContext := context.sub()
	evalErr = n.node.(evalNode).visit(state, &subContext)
	if evalErr != nil {
		return evalErr
	}

	for key := range subContext.resultIter() {
		evalErr = clusterLookup(state, context, key.(string))
		if evalErr != nil {
			return evalErr
		}
	}

	return nil
}

func (n nodeClusterLookup) visit(state *State, context *evalContext) error {
	var evalErr error

	subContext := context.sub()
	evalErr = n.node.(evalNode).visit(state, &subContext)
	if evalErr != nil {
		return evalErr
	}

	keyContext := context.sub()
	evalErr = n.key.(evalNode).visit(state, &keyContext)
	if evalErr != nil {
		return evalErr
	}

	for clusterName := range subContext.resultIter() {
		context.currentClusterName = clusterName.(string)
		for key := range keyContext.resultIter() {
			evalErr = clusterLookup(state, context, key.(string))
			if evalErr != nil {
				return evalErr
			}
		}
	}

	return nil
}

func (c evalContext) sub() evalContext {
	ret := newContext()
	ret.currentClusterName = c.currentClusterName
	ret.depth = c.depth + 1
	return ret
}

func (c evalContext) subCluster(clusterName string) evalContext {
	ret := c.sub()
	ret.currentClusterName = clusterName
	return ret
}

func (n nodeOperator) visit(state *State, context *evalContext) error {
	switch n.op {
	case operatorIntersect:

		leftContext := context.sub()
		if err := n.left.(evalNode).visit(state, &leftContext); err != nil {
			return err
		}

		if leftContext.currentResult.Cardinality() == 0 {
			// Optimization: no need to compute right side if left side is empty
			return nil
		}

		rightContext := context.sub()
		// nodeRegexp needs to know about LHS to filter correctly
		rightContext.workingResult = &leftContext.currentResult
		if err := n.right.(evalNode).visit(state, &rightContext); err != nil {
			return err
		}

		for x := range leftContext.currentResult.Intersect(rightContext.currentResult.Set).Iter() {
			context.addResult(x.(string))
		}
	case operatorSubtract:
		leftContext := context.sub()
		if err := n.left.(evalNode).visit(state, &leftContext); err != nil {
			return err
		}

		if leftContext.currentResult.Cardinality() == 0 {
			// Optimization: no need to compute right side if left side is empty
			return nil
		}

		rightContext := context.sub()
		// nodeRegexp needs to know about LHS to filter correctly
		rightContext.workingResult = &leftContext.currentResult
		if err := n.right.(evalNode).visit(state, &rightContext); err != nil {
			return err
		}

		for x := range leftContext.currentResult.Difference(rightContext.currentResult.Set).Iter() {
			context.addResult(x.(string))
		}
	case operatorUnion:
		if err := n.left.(evalNode).visit(state, context); err != nil {
			return err
		}
		if err := n.right.(evalNode).visit(state, context); err != nil {
			return err
		}
	}
	return nil
}

func (n nodeConstant) visit(state *State, context *evalContext) error {
	context.addResult(n.val)
	return nil
}

var (
	numericRangeRegexp = regexp.MustCompile("^(.*?)(\\d+)\\.\\.([^\\d]*?)?(\\d+)(.*)$")
)

func (n nodeText) visit(state *State, context *evalContext) error {
	match := numericRangeRegexp.FindStringSubmatch(n.val)

	if len(match) == 0 {
		context.addResult(n.val)
		return nil
	}

	leftStr := match[1]
	leftStrToMatch := match[1]
	leftN := match[2]
	rightStr := match[3]
	rightN := match[4]
	trailing := match[5]

	// Equalize the numeric portions. n10..2 will initally be {"n", "10", 2"}, but
	// needs to be converted to {"n1", "0", "2"}.
	for {
		if len(leftN) <= len(rightN) {
			break
		}

		leftStr += leftN[0:1]
		leftN = leftN[1:]
	}

	// a1..a4 is valid, a1..b4 is invalid
	if !strings.HasSuffix(leftStrToMatch, rightStr) {
		context.addResult(n.val)
		return nil
	}

	width := strconv.Itoa(len(leftN))
	low, _ := strconv.Atoi(leftN)
	high, _ := strconv.Atoi(rightN)

	for x := low; x <= high; x++ {
		context.addResult(fmt.Sprintf("%s%0"+width+"d%s", leftStr, x, trailing))
	}

	return nil
}

func (n nodeGroupQuery) visit(state *State, context *evalContext) error {
	subContext := context.sub()
	if err := n.node.(evalNode).visit(state, &subContext); err != nil {
		return err
	}

	lookingFor := subContext.currentResult

	// It's theoretically nicer to re-use clusterLookup here, but it's an order
	// of magnitude slower than poking around the cache directly.
	clusterName := state.defaultCluster

	var cachedClusterDetails cmap.ConcurrentMap
	if tmp, ok := state.clusterCache.Get(clusterName); ok {
		cachedClusterDetails = tmp.(cmap.ConcurrentMap)
	} else {
		cachedClusterDetails = cmap.New()
		state.clusterCache.Set(clusterName, cachedClusterDetails)
	}

	for groupName, group := range state.clusters[state.defaultCluster] {
		key := groupName
		var results *Result
		if tmp, ok := cachedClusterDetails.Get(key); ok {
			results = tmp.(*Result)
		} else {
			subContext := context.sub()
			for _, value := range group {
				err := evalRangeInplace(value, state, &subContext)
				if err != nil {
					return err
				}
			}
			results = &subContext.currentResult
			cachedClusterDetails.Set(key, results)
		}

		for x := range lookingFor.Iter() {
			if results.Contains(x) {
				context.addResult(groupName)
				break
			}
		}
	}
	return nil
}

func (n nodeFunction) visit(state *State, context *evalContext) error {
	switch n.name {
	case "allclusters":
		if err := n.verifyParams(0); err != nil {
			return err
		}
		for clusterKey, _ := range state.clusters {
			context.addResult(clusterKey)
		}
	case "count":
		if err := n.verifyParams(1); err != nil {
			return err
		}
		valueContext := context.sub()
		if err := n.params[0].(evalNode).visit(state, &valueContext); err != nil {
			return err
		}

		context.addResult(strconv.Itoa(valueContext.currentResult.Cardinality()))
	case "has":
		if err := n.verifyParams(2); err != nil {
			return err
		}

		keyContext := context.sub()
		valueContext := context.sub()
		if err := n.params[0].(evalNode).visit(state, &keyContext); err != nil {
			return err
		}
		if err := n.params[1].(evalNode).visit(state, &valueContext); err != nil {
			return err
		}

		key := (<-keyContext.resultIter()).(string)

		for clusterName, _ := range state.clusters {
			subContext := context.subCluster(clusterName)
			clusterLookup(state, &subContext, key)

			l := subContext.currentResult.Set
			r := valueContext.currentResult.Set

			if l.Intersect(r).Cardinality() > 0 {
				context.addResult(clusterName)
			}
		}
	case "clusters":
		if err := n.verifyParams(1); err != nil {
			return err
		}
		subContext := context.sub()
		if err := n.params[0].(evalNode).visit(state, &subContext); err != nil {
			return err
		}

		lookingFor := subContext.currentResult
		context.addSetToResult(state.getResultsFromCachedCQRforSet(lookingFor))
	case "mem":
		if err := n.verifyParams(2); err != nil {
			return err
		}

		clusterContext := context.sub()
		valueContext := context.sub()

		if err := n.params[0].(evalNode).visit(state, &clusterContext); err != nil {
			return err
		}
		if err := n.params[1].(evalNode).visit(state, &valueContext); err != nil {
			return err
		}

		for clusterName := range state.clusters {
			subContext := context.subCluster(clusterName)
			clusterLookup(state, &subContext, "KEYS")

			for _, clusterKey := range subContext.currentResult.Set.ToSlice() {
				clusterKeyContext := subContext.sub()
				clusterLookup(state, &clusterKeyContext, clusterKey.(string))

				if clusterKeyContext.currentResult.Set.Intersect(valueContext.currentResult.Set).Cardinality() > 0 {
					context.addResult(clusterKey.(string))
				}
			}
		}
	default:
		return errors.New(fmt.Sprintf("Unknown function: %s", n.name))
	}
	return nil
}

func (n nodeFunction) verifyParams(expected int) error {
	if len(n.params) != expected {
		msg := fmt.Sprintf("Wrong number of params for %s: expected %d, got %d.",
			n.name,
			expected,
			len(n.params),
		)
		return errors.New(msg)
	}
	return nil
}

func (n nodeRegexp) visit(state *State, context *evalContext) error {
	if context.workingResult == nil {
		subContext := context.sub()
		state.allValues(&subContext)
		context.workingResult = &subContext.currentResult
	}

	r, err := regexp.Compile(n.val)

	if err != nil {
		return err
	}

	for x := range context.workingResult.Iter() {
		if r.MatchString(x.(string)) {
			context.addResult(x.(string))
		}
	}

	return nil
}

func (n nodeNull) visit(state *State, context *evalContext) error {
	return nil
}

func (state *State) allValues(context *evalContext) error {
	// Expand everything into the set
	return evalRangeInplace("@{%"+state.defaultCluster+":KEYS}", state, context)
}

func clusterLookup(state *State, context *evalContext, key string) error {
	var evalErr error
	clusterName := context.currentClusterName
	if clusterName == "" {
		clusterName = state.defaultCluster
	}
	cluster := state.clusters[clusterName]

	if key == "KEYS" {
		for k, _ := range cluster {
			context.currentResult.Add(k) // TODO: addResult
		}
		return nil
	}

	var cachedClusterDetails cmap.ConcurrentMap
	if tmp, ok := state.clusterCache.Get(clusterName); ok {
		cachedClusterDetails = tmp.(cmap.ConcurrentMap)
	} else {
		cachedClusterDetails = cmap.New()
		state.clusterCache.Set(clusterName, cachedClusterDetails)
	}
	var results *Result
	if tmp, ok := cachedClusterDetails.Get(key); ok {
		results = tmp.(*Result)
	} else {
		clusterExp := cluster[key] // TODO: Error handling

		subContext := context.subCluster(context.currentClusterName)

		for _, value := range clusterExp {
			evalErr = evalRangeInplace(value, state, &subContext)
			if evalErr != nil {
				return evalErr
			}
		}
		results = &subContext.currentResult
		cachedClusterDetails.Set(key, results)
	}

	for x := range results.Iter() {
		context.addResult(x.(string))
	}
	return nil
}

func (c *evalContext) addResult(value string) {
	if c.currentResult.Cardinality() >= MaxResults {
		panic(tooManyResults{})
	}

	if len(value) > MaxQuerySize {
		panic(errors.New(
			fmt.Sprintf("Value would exceed max query size: %s...", value[0:20])))
	}

	c.currentResult.Add(value)
}

func (c *evalContext) addSetToResult(set mapset.Set) {
	if c.currentResult.Cardinality()+set.Cardinality() >= MaxResults {
		panic(tooManyResults{})
	}
	for value := range set.Iter() {
		c.currentResult.Add(value.(string))
	}
}

func (c *evalContext) resultIter() <-chan interface{} {
	return c.currentResult.Iter()
}

type evalNode interface {
	visit(*State, *evalContext) error
}

func (state *State) getResultsFromCachedCQRforSet(set mapset.Set) Result {
	context := newContext()
	state.populateCachedCQRforSet(set)
	for name := range set.Iter() {
		if subResult, ok := state.cachedCQR.Get(name.(string)); ok {
			//add subresults to context.currentResult
			context.addSetToResult(subResult.(Result))
		}
	}
	return context.currentResult
}
