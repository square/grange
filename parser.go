package grange

type Node interface {
	merge(Node) Node
}

type NullNode struct{}

type TextNode struct {
	val string
}

type ClusterLookupNode struct {
	name string
	key  string
}

type IntersectNode struct {
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

func (n GroupNode) merge(other Node) Node {
	return GroupNode{n.head.merge(other), n.tail.merge(other)}
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

func (n ClusterLookupNode) merge(other Node) Node {
	return n
}

func (n ErrorNode) merge(other Node) Node {
	return n
}

func (n IntersectNode) merge(other Node) Node {
	panic("how did you even get here")
}

func parseRange(items chan item) Node {
	var currentNode Node
	var subNode Node

	for currentItem := range items {
		switch currentItem.typ {
		case itemText:
			if currentNode != nil {
				currentNode = currentNode.merge(TextNode{currentItem.val})
			} else {
				currentNode = TextNode{currentItem.val}
			}
		case itemCluster:
			currentNode = parseCluster(items)
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
		case itemIntersect:
			if currentNode == nil {
				currentNode = ErrorNode{"No left side provided for intersection"}
			}

			return IntersectNode{currentNode, parseRange(items)}
		}
	}
	return currentNode
}

func parseCluster(items chan item) Node {
	item := <-items
	clusterKey := "CLUSTER" // Default

	if item.typ == itemText {
		clusterName := item.val

		item = <-items
		if item.typ == itemClusterKey {
			item = <-items

			if item.typ == itemText {
				clusterKey = item.val
			} else {
				//panic("unimplemented")
			}
		} else if item.typ != itemEOF {
			panic("unimplemented")
		}

		return ClusterLookupNode{clusterName, clusterKey}
	} else {
		panic("unimplemented")
	}
}
