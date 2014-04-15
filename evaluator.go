package grange

import (
	"errors"
	"fmt"
)

type Cluster map[string][]string

type RangeState struct {
	clusters map[string]Cluster
}

type evalContext struct {
	currentClusterName string
}

func AddCluster(state RangeState, name string, c Cluster) {
	state.clusters[name] = c
}

func NewState() RangeState {
	return RangeState{
		clusters: map[string]Cluster{},
	}
}

func EvalRange(input string, state *RangeState) (result []string, err error) {
	return evalRange(input, state)
}

func evalRange(input string, state *RangeState) (result []string, err error) {
	return evalRangeWithContext(input, state, &evalContext{})
}

func evalRangeWithContext(input string, state *RangeState, context *evalContext) (result []string, err error) {
	_, items := lexRange("eval", input)

	node := parseRange(items).(EvalNode)
	parseError := node.findError()
	if parseError != nil {
		return nil, parseError
	}
	//fmt.Printf("%s\n", node)

	return node.visit(state, context), nil
}

func (n ClusterLookupNode) visit(state *RangeState, _ *evalContext) []string {
	return clusterLookup(state, n.name, n.key)
}

func (n LocalClusterLookupNode) visit(state *RangeState, context *evalContext) []string {
	if context.currentClusterName == "" {
		panic("Unimplemented")
	}

	return clusterLookup(state, context.currentClusterName, n.key)
}

func (n IntersectNode) visit(state *RangeState, context *evalContext) []string {
	result := []string{}
	leftSide := n.left.(EvalNode).visit(state, context)

	if len(leftSide) == 0 {
		// Optimization: no need to compute right side if left side is empty
		return result
	}

	rightSide := n.right.(EvalNode).visit(state, context)

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

func (n TextNode) visit(state *RangeState, context *evalContext) []string {
	return []string{n.val}
}

func (n GroupNode) visit(state *RangeState, context *evalContext) []string {
	return append(
		n.head.(EvalNode).visit(state, context),
		n.tail.(EvalNode).visit(state, context)...,
	)
}

func (n HasNode) visit(state *RangeState, context *evalContext) []string {
	result := []string{}

	for clusterName, cluster := range state.clusters {
		values := cluster[n.key]

		if values != nil {
			for _, value := range values {
				if value == n.match {
					result = append(result, clusterName)
				}
			}
		}
	}

	return result
}

func (n ErrorNode) visit(state *RangeState, context *evalContext) []string {
	panic("should not happen")
}

func clusterLookup(state *RangeState, clusterName string, key string) []string {
	clusterExp := state.clusters[clusterName][key] // TODO: Error handling
	result := []string{}

	for _, value := range clusterExp {
		expansion, _ := evalRangeWithContext(value, state, &evalContext{
			currentClusterName: clusterName,
		})
		result = append(result, expansion...)
	}
	return result
}

func (n IntersectNode) String() string {
	return fmt.Sprintf("<%s & %s>", n.left, n.right)
}

func (n ClusterLookupNode) String() string {
	return fmt.Sprintf("%%%s:%s", n.name, n.key)
}

func (n LocalClusterLookupNode) String() string {
	return fmt.Sprintf("$%s", n.key)
}

func (n TextNode) String() string {
	return fmt.Sprintf("%s", n.val)
}

func (n HasNode) String() string {
	return fmt.Sprintf("has(%s;%s)", n.key, n.match)
}

func (n ErrorNode) findError() error {
	return errors.New(n.message)
}

// TODO: Better way to do this?
func (TextNode) findError() error               { return nil }
func (ClusterLookupNode) findError() error      { return nil }
func (LocalClusterLookupNode) findError() error { return nil }
func (n GroupNode) findError() error {
	err := n.head.(EvalNode).findError()
	if err != nil {
		return err
	}
	return n.tail.(EvalNode).findError()
}
func (HasNode) findError() error { return nil }

func (n IntersectNode) findError() error {
	err := n.left.(EvalNode).findError()
	if err != nil {
		return err
	}
	return n.right.(EvalNode).findError()
}

type EvalNode interface {
	visit(*RangeState, *evalContext) []string
	findError() error
}
