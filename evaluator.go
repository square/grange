package grange

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"

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
	clusterCache map[string]map[string]*Result
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
	// result sizes to approximately 1MB.
	MaxQuerySize = 1000

	// The maximum number of results a query can return. Execution will be
	// short-circuited once this many results have been gathered. No error will
	// be returned.
	MaxResults = 10000

	// The default cluster for new states, used by @ and ? syntax. Can be changed
	// per-state using SetDefaultCluster.
	DefaultCluster = "GROUPS"
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
	}
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
func (state *State) PrimeCache() {
	// traverse and expand every cluster, adding them to cache.
	state.Query("clusters(a)")

	// traverse and expand every group
	state.Query("//")
}

// ResetCache clears cached expansions. The public API for modifying state
// already calls this when necessary, so you shouldn't really have a need to
// call this.
func (state *State) ResetCache() {
	state.clusterCache = map[string]map[string]*Result{}
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

type tooManyResults struct{}

type evalContext struct {
	currentClusterName string
	currentResult      Result
	workingResult      *Result
}

func newContext() evalContext {
	return evalContext{currentResult: NewResult()}
}

func newClusterContext(clusterName string) evalContext {
	return evalContext{
		currentClusterName: clusterName,
		currentResult:      NewResult(),
	}
}

func parseRange(input string) (parserNode, error) {
	r := &rangeQuery{Buffer: input}
	r.Init()
	if err := r.Parse(); err != nil {
		return nil, err
	}
	r.Execute()
	return r.nodeStack[0], nil
}

func evalRangeWithContext(input string, state *State, context *evalContext) (Result, error) {
	err := evalRangeInplace(input, state, context)

	return context.currentResult, err
}

// Useful internally so that results do not need to be copied all over the place
func evalRangeInplace(input string, state *State, context *evalContext) (err error) {
	node, parseError := parseRange(input)
	if parseError != nil {
		return errors.New("Could not parse query")
	}

	defer func() {
		if r := recover(); r != nil {
			switch r.(type) {
			case tooManyResults:
				// No error returned, we just chop off the results
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
	leftContext := newContext()
	rightContext := newContext()
	middleContext := newContext()
	// TODO: Handle errors
	n.left.(evalNode).visit(state, &leftContext)
	n.node.(evalNode).visit(state, &middleContext)
	n.right.(evalNode).visit(state, &rightContext)

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

func (n nodeLocalClusterLookup) visit(state *State, context *evalContext) error {
	return clusterLookup(state, context, n.key)
}

func (n nodeClusterLookup) visit(state *State, context *evalContext) error {
	var evalErr error

	subContext := newContext()
	evalErr = n.node.(evalNode).visit(state, &subContext)
	if evalErr != nil {
		return evalErr
	}

	keyContext := newContext()
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
	return newClusterContext(c.currentClusterName)
}

func (n nodeOperator) visit(state *State, context *evalContext) error {
	switch n.op {
	case operatorIntersect:

		leftContext := context.sub()
		n.left.(evalNode).visit(state, &leftContext) // TODO: Error handle

		if leftContext.currentResult.Cardinality() == 0 {
			// Optimization: no need to compute right side if left side is empty
			return nil
		}

		rightContext := context.sub()
		// nodeRegexp needs to know about LHS to filter correctly
		rightContext.workingResult = &leftContext.currentResult
		n.right.(evalNode).visit(state, &rightContext) // TODO: Error handle

		for x := range leftContext.currentResult.Intersect(rightContext.currentResult.Set).Iter() {
			context.addResult(x.(string))
		}
	case operatorSubtract:
		leftContext := context.sub()
		n.left.(evalNode).visit(state, &leftContext) // TODO: Error handle

		if leftContext.currentResult.Cardinality() == 0 {
			// Optimization: no need to compute right side if left side is empty
			return nil
		}

		rightContext := context.sub()
		// nodeRegexp needs to know about LHS to filter correctly
		rightContext.workingResult = &leftContext.currentResult
		n.right.(evalNode).visit(state, &rightContext) // TODO: Error handle

		for x := range leftContext.currentResult.Difference(rightContext.currentResult.Set).Iter() {
			context.addResult(x.(string))
		}
	case operatorUnion:
		// TODO: Handle errors
		n.left.(evalNode).visit(state, context)
		n.right.(evalNode).visit(state, context)
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

	for {
		if len(leftN) <= len(rightN) {
			break
		}

		leftStr += leftN[0:1]
		leftN = leftN[1:]
	}

	// a1..a4 is valid, a1..b4 is invalid
	if len(rightStr) != 0 && leftStrToMatch != rightStr {
		context.addResult(n.val)
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
	subContext := newContext()
	// TODO: Handle errors
	n.node.(evalNode).visit(state, &subContext)
	lookingFor := subContext.currentResult

	for groupName, group := range state.clusters[state.defaultCluster] {
		groupContext := newContext()
		for _, value := range group {
			// TODO: Handle errors
			evalRangeInplace(value, state, &groupContext)
		}

		for x := range lookingFor.Iter() {
			if groupContext.currentResult.Contains(x) {
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
		for clusterKey, _ := range state.clusters {
			context.addResult(clusterKey)
		}
	case "count":
		valueContext := newContext()
		n.params[0].(evalNode).visit(state, &valueContext)

		context.addResult(strconv.Itoa(valueContext.currentResult.Cardinality()))
	case "has":
		// TODO: Error handling when no or multiple results
		keyContext := newContext()
		valueContext := newContext()
		n.params[0].(evalNode).visit(state, &keyContext)
		n.params[1].(evalNode).visit(state, &valueContext)

		key := (<-keyContext.resultIter()).(string)
		toMatch := (<-valueContext.resultIter()).(string)

		for clusterName, cluster := range state.clusters {
			for _, value := range cluster[key] {
				// TODO: Need to eval value?
				if value == toMatch {
					context.addResult(clusterName)
				}
			}
		}
	case "clusters":
		// TODO: Error handling
		subContext := newContext()
		n.params[0].(evalNode).visit(state, &subContext)

		lookingFor := subContext.currentResult

		for clusterName, _ := range state.clusters {
			subContext = newClusterContext(clusterName)
			clusterLookup(state, &subContext, "CLUSTER")

			for value := range subContext.resultIter() {
				if lookingFor.Contains(value) {
					context.addResult(clusterName)
				}
			}
		}
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

	if state.clusterCache[clusterName] == nil {
		state.clusterCache[clusterName] = map[string]*Result{}
	}

	if state.clusterCache[clusterName][key] == nil {
		clusterExp := cluster[key] // TODO: Error handling

		subContext := newClusterContext(context.currentClusterName)

		for _, value := range clusterExp {
			evalErr = evalRangeInplace(value, state, &subContext)
			if evalErr != nil {
				return evalErr
			}
		}

		state.clusterCache[clusterName][key] = &subContext.currentResult
	}

	for x := range state.clusterCache[clusterName][key].Iter() {
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

func (c *evalContext) resultIter() <-chan interface{} {
	return c.currentResult.Iter()
}

type evalNode interface {
	visit(*State, *evalContext) error
}
