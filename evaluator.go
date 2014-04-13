package grange

import (
	"fmt"
)

type cluster map[string][]string

type rangeState struct {
	clusters map[string]cluster
}

func evalRange(input string, state *rangeState) []string {
	_, items := lexRange("eval", input)

	node := parseRange(items).(EvalNode)

	return node.visit(state)
}

func (n ClusterLookupNode) visit(state *rangeState) []string {
	return clusterLookup(state, n.name, n.key)
}

func (n IntersectNode) visit(state *rangeState) []string {
	result := []string{}
	leftSide := n.left.(EvalNode).visit(state)

	if len(leftSide) == 0 {
		// Optimization: no need to compute right side if left side is empty
		return result
	}

	rightSide := n.right.(EvalNode).visit(state)

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
}

func (n TextNode) visit(state *rangeState) []string {
	return []string{n.val}
}

func clusterLookup(state *rangeState, clusterName string, key string) []string {
	return state.clusters[clusterName][key] // TODO: Error handling
}

func (n IntersectNode) String() string {
	return fmt.Sprintf("<%s & %s>", n.left, n.right)
}

func (n ClusterLookupNode) String() string {
	return fmt.Sprintf("%%%s:%s", n.name, n.key)
}

func (n TextNode) String() string {
	return fmt.Sprintf("%s", n.val)
}

type EvalNode interface {
	visit(*rangeState) []string
}
