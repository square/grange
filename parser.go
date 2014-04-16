package grange

import (
	"fmt"
	"strings"
)

// TODO: Don't export these node types
type Node interface {
	merge(Node) Node
}

type NullNode struct{}

type TextNode struct {
	val string
}

type MatchNode struct {
	val string
}

type ClusterLookupNode struct {
	name string
	key  string
}

type GroupLookupNode struct {
	name string
}

type SubexprNode struct {
	expr Node
	key  string
}

type IntersectNode struct {
	left  Node
	right Node
}

type ExcludeNode struct {
	left  Node
	right Node
}

type ErrorNode struct {
	message string
}

type GroupNode struct {
	head Node
	tail Node
}

type HasNode struct {
	key   string
	match string
}

type LocalClusterLookupNode struct {
	key string
}

func (n GroupNode) merge(other Node) Node {
	return GroupNode{n.head.merge(other), n.tail.merge(other)}
}

func (n SubexprNode) merge(other Node) Node {
	return n // TODO: what does this even mean
}

func (n TextNode) merge(other Node) Node {
	switch other.(type) {
	case TextNode:
		return TextNode{n.val + other.(TextNode).val}
	case GroupNode:
		group := other.(GroupNode)
		return GroupNode{n.merge(group.head), n.merge(group.tail)}
	default:
		return n
	}
}

func (n MatchNode) merge(other Node) Node {
	return n // TODO: what?
}

func (n ClusterLookupNode) merge(other Node) Node {
	return n
}

func (n LocalClusterLookupNode) merge(other Node) Node {
	return n
}

func (n GroupLookupNode) merge(other Node) Node {
	return n
}

func (n ErrorNode) merge(other Node) Node {
	return n
}

func (n HasNode) merge(other Node) Node {
	return n
}

func (n IntersectNode) merge(other Node) Node {
	panic("how did you even get here")
}

func (n ExcludeNode) merge(other Node) Node {
	panic("how did you even get here")
}

func parseRange(items chan item) Node {
	var currentNode Node
	var subNode Node

	for currentItem := range items {
		//fmt.Printf("Parse item: %s\n", currentItem)
		switch currentItem.typ {
		case itemText:
			if currentNode != nil {
				currentNode = currentNode.merge(TextNode{currentItem.val})
			} else {
				currentNode = TextNode{currentItem.val}
			}
		case itemGroupLookup:
			currentNode = GroupLookupNode{currentItem.val}
		case itemFunctionName:
			if currentItem.val != "has" {
				return ErrorNode{fmt.Sprintf("Unknown function: %s", currentItem.val)}
			}

			paramItem := <-items

			if paramItem.typ != itemFunctionParam {
				return ErrorNode{fmt.Sprintf("Expecting parameter to function %s", currentItem.val)}
			} else {
				functionParam := paramItem.val

				tokens := strings.Split(functionParam, ";")

				if len(tokens) != 2 {
					return ErrorNode{fmt.Sprintf("Invalid function parameter: %s", functionParam)}
				}

				currentNode = HasNode{tokens[0], tokens[1]}
			}
		case itemCluster:
			currentNode = ClusterLookupNode{currentItem.val, "CLUSTER"}
		case itemClusterKey:
			switch currentNode.(type) {
			case ClusterLookupNode:
				n := currentNode.(ClusterLookupNode)
				currentNode = ClusterLookupNode{n.name, currentItem.val}
			case SubexprNode:
				n := currentNode.(SubexprNode)
				currentNode = SubexprNode{n.expr, currentItem.val}
			default:
				return ErrorNode{fmt.Sprintf(":%s must follow a cluster", currentItem.val)}
			}
		case itemLocalClusterKey:
			currentNode = LocalClusterLookupNode{currentItem.val}
		case itemLeftGroup:
			// Find closing right group
			stack := 1
			subitems := make(chan item, 1000)
			subparse := true
			for subparse {
				subItem := <-items

				switch subItem.typ {
				case itemEOF:
					return ErrorNode{"No matching closing bracket"}
				case itemLeftGroup:
					stack++
				case itemRightGroup:
					stack--
					if stack == 0 {
						subitems <- item{itemEOF, ""}
						close(subitems)
						subNode = parseRange(subitems)
						subparse = false
					}
				}

				if !subparse {
					break
				}
				subitems <- subItem
			}
			if currentNode != nil {
				currentNode = currentNode.merge(subNode)
			} else {
				currentNode = subNode
			}
		case itemComma:
			if currentNode != nil {
				return GroupNode{currentNode, parseRange(items)}
			}
		case itemSubexprStart:
			// TODO: Merge
			currentNode = parseSubexpr(items)
		case itemMatch:
			// TODO: Merge
			currentNode = MatchNode{currentItem.val}
		case itemIntersect:
			if currentNode == nil {
				currentNode = ErrorNode{"No left side provided for intersection"}
			}

			return IntersectNode{currentNode, parseRange(items)}
		case itemExclude:
			if currentNode == nil {
				currentNode = ErrorNode{"No left side provided for exclusion"}
			}

			return ExcludeNode{currentNode, parseRange(items)}
		case itemEOF:
			return currentNode
		}
	}
	panic("Unreachable")
}

func parseSubexpr(items chan item) Node {
	subItems := make(chan item, 1000) // TODO: Don't use a bounded channel like this

	for currentItem := range items {
		switch currentItem.typ {
		case itemSubexprEnd:
			subItems <- item{itemEOF, ""}
			return SubexprNode{parseRange(subItems), "CLUSTER"}
		case itemEOF:
			return ErrorNode{"Could not find end of subexpr"}
		default:
			subItems <- currentItem
		}
	}

	panic("Unreachable")
}
