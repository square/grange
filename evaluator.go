package grange

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/deckarep/golang-set"
)

type Cluster map[string][]string

type RangeState struct {
	clusters map[string]Cluster
	groups   Cluster

	// Populated lazily as groups are evaluated. They won't change unless state
	// changes.
	groupCache map[string]*mapset.Set
}

type evalContext struct {
	currentClusterName string
	currentResult      mapset.Set
	workingResult      *mapset.Set
}

func newContext() evalContext {
	return evalContext{currentResult: mapset.NewSet()}
}

func newClusterContext(clusterName string) evalContext {
	return evalContext{
		currentClusterName: clusterName,
		currentResult:      mapset.NewSet(),
	}
}

func SetGroups(state *RangeState, c Cluster) {
	state.groups = c
	state.ResetCache()
}

func AddCluster(state *RangeState, name string, c Cluster) {
	state.clusters[name] = c
	state.ResetCache()
}

func (state *RangeState) ResetCache() {
	state.groupCache = map[string]*mapset.Set{}
}

func NewState() RangeState {
	return RangeState{
		clusters:   map[string]Cluster{},
		groups:     Cluster{},
		groupCache: map[string]*mapset.Set{},
	}
}

func NewResult(args ...interface{}) mapset.Set {
	return mapset.NewSetFromSlice(args)
}

func parseRange(input string) (Node, error) {
	r := &RangeQuery{Buffer: input}
	r.Init()
	if err := r.Parse(); err != nil {
		return nil, err
	}
	r.Execute()
	return r.nodeStack[0], nil
}

func EvalRange(input string, state *RangeState) (result mapset.Set, err error) {
	return evalRange(input, state)
}

func evalRange(input string, state *RangeState) (result mapset.Set, err error) {
	context := newContext()
	return evalRangeWithContext(input, state, &context)
}

func evalRangeWithContext(input string, state *RangeState, context *evalContext) (result mapset.Set, err error) {
	parseError := evalRangeInplace(input, state, context)

	return context.currentResult, parseError
}

// Useful internally so that results do not need to be copied all over the place
func evalRangeInplace(input string, state *RangeState, context *evalContext) error {
	node, parseError := parseRange(input)
	if parseError != nil {
		return parseError
	}

	node.(EvalNode).visit(state, context)
	return nil
}

func (c evalContext) hasResults() bool {
	return c.currentResult.Cardinality() == 0
}

func (n BracesNode) visit(state *RangeState, context *evalContext) error {
	leftContext := newContext()
	rightContext := newContext()
	middleContext := newContext()
	// TODO: Handle errors
	n.left.(EvalNode).visit(state, &leftContext)
	n.node.(EvalNode).visit(state, &middleContext)
	n.right.(EvalNode).visit(state, &rightContext)

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

func (n LocalClusterLookupNode) visit(state *RangeState, context *evalContext) error {
	if context.currentClusterName == "" {
		return groupLookup(state, context, n.key)
	}

	return clusterLookup(state, context, n.key)
}

func (n ClusterLookupNode) visit(state *RangeState, context *evalContext) error {
	subContext := newContext()
	n.node.(EvalNode).visit(state, &subContext)

	for clusterName := range subContext.resultIter() {
		context.currentClusterName = clusterName.(string)
		clusterLookup(state, context, n.key)
	}

	return nil
}

func (n GroupLookupNode) visit(state *RangeState, context *evalContext) error {
	subContext := context.sub()
	n.node.(EvalNode).visit(state, &subContext) // TODO: Error handle

	for key := range subContext.resultIter() {
		groupLookup(state, context, key.(string))
	}

	return nil
}

func (c evalContext) sub() evalContext {
	return newClusterContext(c.currentClusterName)
}

func (n OperatorNode) visit(state *RangeState, context *evalContext) error {
	switch n.op {
	case operatorIntersect:

		leftContext := context.sub()
		n.left.(EvalNode).visit(state, &leftContext) // TODO: Error handle

		if leftContext.currentResult.Cardinality() == 0 {
			// Optimization: no need to compute right side if left side is empty
			return nil
		}

		rightContext := context.sub()
		// RegexNode needs to know about LHS to filter correctly
		rightContext.workingResult = &leftContext.currentResult
		n.right.(EvalNode).visit(state, &rightContext) // TODO: Error handle

		for x := range leftContext.currentResult.Intersect(rightContext.currentResult).Iter() {
			context.addResult(x.(string))
		}
	case operatorSubtract:
		leftContext := context.sub()
		n.left.(EvalNode).visit(state, &leftContext) // TODO: Error handle

		if leftContext.currentResult.Cardinality() == 0 {
			// Optimization: no need to compute right side if left side is empty
			return nil
		}

		rightContext := context.sub()
		// RegexNode needs to know about LHS to filter correctly
		rightContext.workingResult = &leftContext.currentResult
		n.right.(EvalNode).visit(state, &rightContext) // TODO: Error handle

		for x := range leftContext.currentResult.Difference(rightContext.currentResult).Iter() {
			context.addResult(x.(string))
		}
	case operatorUnion:
		// TODO: Handle errors
		n.left.(EvalNode).visit(state, context)
		n.right.(EvalNode).visit(state, context)
	}
	return nil
}

func (n ConstantNode) visit(state *RangeState, context *evalContext) error {
	context.addResult(n.val)
	return nil
}

var (
	numericRangeRegexp = regexp.MustCompile("^(.*?)(\\d+)\\.\\.([^\\d]*?)?(\\d+)(.*)$")
)

func (n TextNode) visit(state *RangeState, context *evalContext) error {
	match := numericRangeRegexp.FindStringSubmatch(n.val)

	if len(match) == 0 {
		context.currentResult.Add(n.val)
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
		context.currentResult.Add(n.val)
		return nil
	}

	width := strconv.Itoa(len(leftN))
	low, _ := strconv.Atoi(leftN)
	high, _ := strconv.Atoi(rightN)

	for x := low; x <= high; x++ {
		context.currentResult.Add(fmt.Sprintf("%s%0"+width+"d%s", leftStr, x, trailing))
	}

	return nil
}

func (n GroupQueryNode) visit(state *RangeState, context *evalContext) error {
	subContext := newContext()
	// TODO: Handle errors
	n.node.(EvalNode).visit(state, &subContext)
	lookingFor := subContext.currentResult

	for groupName, group := range state.groups {
		groupContext := newContext()
		for _, value := range group {
			// TODO: Handle errors
			evalRangeInplace(value, state, &groupContext)
		}

		for x := range lookingFor {
			if groupContext.currentResult.Contains(x) {
				context.addResult(groupName)
				break
			}
		}
	}
	return nil
}

func (n FunctionNode) visit(state *RangeState, context *evalContext) error {
	switch n.name {
	case "has":
		// TODO: Error handling when no or multiple results
		keyContext := newContext()
		valueContext := newContext()
		n.params[0].(EvalNode).visit(state, &keyContext)
		n.params[1].(EvalNode).visit(state, &valueContext)

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
		n.params[0].(EvalNode).visit(state, &subContext)

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

func (n RegexNode) visit(state *RangeState, context *evalContext) error {
	if context.workingResult == nil {
		subContext := context.sub()
		state.allValues(&subContext)
		context.workingResult = &subContext.currentResult
	}

	for x := range context.workingResult.Iter() {
		if strings.Contains(x.(string), n.val) {
			context.addResult(x.(string))
		}
	}

	return nil
}

func (n NullNode) visit(state *RangeState, context *evalContext) error {
	return nil
}

func (state *RangeState) allValues(context *evalContext) error {
	// Expand everything into the set
	for _, v := range state.groups {
		for _, subv := range v {
			// TODO: Handle errors
			evalRangeInplace(subv, state, context)
		}
	}

	return nil
}

func groupLookup(state *RangeState, context *evalContext, key string) error {
	if state.groupCache[key] != nil {
		for x := range state.groupCache[key].Iter() {
			context.addResult(x.(string))
		}
		return nil
	}

	clusterExp := state.groups[key]

	for _, value := range clusterExp {
		// TODO: Return errors correctly
		evalRangeInplace(value, state, context)
	}
	state.groupCache[key] = &context.currentResult
	return nil
}

func clusterLookup(state *RangeState, context *evalContext, key string) error {
	cluster := state.clusters[context.currentClusterName]

	if key == "KEYS" {
		for k, _ := range cluster {
			context.currentResult.Add(k)
		}
		return nil
	}

	clusterExp := cluster[key] // TODO: Error handling

	subContext := newClusterContext(context.currentClusterName)

	for _, value := range clusterExp {
		evalRangeInplace(value, state, &subContext)
	}
	for x := range subContext.currentResult.Iter() {
		context.addResult(x.(string))
	}
	return nil
}

func (c *evalContext) addResult(value string) {
	c.currentResult.Add(value)
}

func (c *evalContext) resultIter() <-chan interface{} {
	return c.currentResult.Iter()
}

type EvalNode interface {
	visit(*RangeState, *evalContext) error
}
