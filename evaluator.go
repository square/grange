package grange

import (
	"strings"
)

type Cluster map[string][]string

type RangeState struct {
	clusters map[string]Cluster
	groups   Cluster
}

type evalContext struct {
	currentClusterName string
	currentResult      []string
}

func SetGroups(state *RangeState, c Cluster) {
	state.groups = c
}

func AddCluster(state *RangeState, name string, c Cluster) {
	state.clusters[name] = c
}

func NewState() RangeState {
	return RangeState{
		clusters: map[string]Cluster{},
	}
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

func EvalRange(input string, state *RangeState) (result []string, err error) {
	return evalRange(input, state)
}

func evalRange(input string, state *RangeState) (result []string, err error) {
	return evalRangeWithContext(input, state, &evalContext{})
}

func evalRangeWithContext(input string, state *RangeState, context *evalContext) (result []string, err error) {
	node, parseError := parseRange(input)
	if parseError != nil {
		return []string{}, parseError
	}

	return node.(EvalNode).visit(state, context), nil
}

func (n ClusterLookupNode) visit(state *RangeState, _ *evalContext) []string {
	return clusterLookup(state, n.name, n.key)
}

func (n LocalClusterLookupNode) visit(state *RangeState, context *evalContext) []string {
	if context.currentClusterName == "" {
		return groupLookup(state, n.key)
	}

	return clusterLookup(state, context.currentClusterName, n.key)
}

func (n SubexprNode) visit(state *RangeState, context *evalContext) []string {
	clusters := n.node.(EvalNode).visit(state, context)
	accum := map[string]bool{}

	for _, cluster := range clusters {
		for _, x := range clusterLookup(state, cluster, n.key) {
			accum[x] = true
		}
	}
	result := []string{}
	for x, _ := range accum {
		result = append(result, x)
	}
	return result
}

func (n GroupLookupNode) visit(state *RangeState, _ *evalContext) []string {
	return groupLookup(state, n.name)
}

func (n OperatorNode) visit(state *RangeState, context *evalContext) []string {
	switch n.op {
	case operatorIntersect:

		result := []string{}
		leftSide := n.left.(EvalNode).visit(state, context)

		if len(leftSide) == 0 {
			// Optimization: no need to compute right side if left side is empty
			return result
		}

		context.currentResult = leftSide
		rightSide := n.right.(EvalNode).visit(state, context)
		context.currentResult = nil

		set := map[string]bool{}
		for _, x := range leftSide {
			set[x] = true
		}
		for _, y := range rightSide {
			if len(result) == len(leftSide) {
				// Optimization: early exit when all results have been computed.
				break
			}

			if set[y] {
				result = append(result, y)
			}
		}
		return result

	case operatorSubtract:
		result := []string{}
		leftSide := n.left.(EvalNode).visit(state, context)

		if len(leftSide) == 0 {
			// Optimization: no need to compute right side if left side is empty
			return result
		}

		context.currentResult = leftSide
		rightSide := n.right.(EvalNode).visit(state, context)
		context.currentResult = nil

		set := map[string]bool{}
		for _, x := range rightSide {
			set[x] = true
		}
		for _, y := range leftSide {
			if !set[y] {
				result = append(result, y)
			}
		}
		return result
	case operatorUnion:
		result := []string{}
		leftSide := n.left.(EvalNode).visit(state, context)
		rightSide := n.right.(EvalNode).visit(state, context)

		set := map[string]bool{}
		for _, x := range leftSide {
			set[x] = true
		}
		for _, x := range rightSide {
			set[x] = true
		}
		for x, _ := range set {
			result = append(result, x)
		}
		return result
	}
	return []string{}
}

func (n TextNode) visit(state *RangeState, context *evalContext) []string {
	return []string{n.val}
}

/*
func (n GroupNode) visit(state *RangeState, context *evalContext) []string {
	return append(
		n.head.(EvalNode).visit(state, context),
		n.tail.(EvalNode).visit(state, context)...,
	)
}
*/

func (n FunctionNode) visit(state *RangeState, context *evalContext) []string {
	switch n.name {
	case "has":
		result := []string{}

		for clusterName, cluster := range state.clusters {
			// TODO: Error handling when no or multiple results
			values := cluster[n.params[0].(EvalNode).visit(state, context)[0]]

			if values != nil {
				for _, value := range values {
					// TODO: Error handling when no or multiple results
					if value == n.params[1].(EvalNode).visit(state, context)[0] {
						result = append(result, clusterName)
					}
				}
			}
		}

		return result
	}
	return []string{}
}

func (n RegexNode) visit(state *RangeState, context *evalContext) []string {
	var toMatch []string
	result := []string{}
	if context.currentResult != nil {
		toMatch = context.currentResult
	} else {
		toMatch = state.allValues()
	}

	for _, x := range toMatch {
		if strings.Contains(x, n.val) {
			result = append(result, x)
		}
	}

	return result
}

func (state *RangeState) allValues() []string {
	// Fake set
	accum := map[string]bool{}

	// Expand everything into the set
	for _, v := range state.groups {
		for _, subv := range v {
			expansion, err := evalRange(subv, state)

			// TODO: Ignoring errors, probably should get rid of them on initial load.
			if err == nil {
				for _, x := range expansion {
					accum[x] = true
				}
			}
		}
	}

	// Keys of map are the set
	result := []string{}
	for k, _ := range accum {
		result = append(result, k)
	}
	return result
}

func groupLookup(state *RangeState, key string) []string {
	clusterExp := state.groups[key]

	result := []string{}

	for _, value := range clusterExp {
		expansion, _ := evalRange(value, state)
		result = append(result, expansion...)
	}
	return result
}

func clusterLookup(state *RangeState, clusterName string, key string) []string {
	cluster := state.clusters[clusterName]
	if key == "KEYS" {
		keys := []string{}
		for k, _ := range cluster {
			keys = append(keys, k)
		}
		return keys
	}

	clusterExp := cluster[key] // TODO: Error handling
	result := []string{}

	for _, value := range clusterExp {
		expansion, _ := evalRangeWithContext(value, state, &evalContext{
			currentClusterName: clusterName,
		})
		result = append(result, expansion...)
	}
	return result
}

type EvalNode interface {
	visit(*RangeState, *evalContext) []string
}
