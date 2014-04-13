package grange

type Node interface {
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

func parseRange(items chan item) Node {
	var currentNode Node

	for item := range items {
		switch item.typ {
		case itemText:
			currentNode = TextNode{item.val}
		case itemCluster:
			currentNode = parseCluster(items)
		case itemIntersect:
			if currentNode != nil {
				return IntersectNode{currentNode, parseRange(items)}
			} else {
				panic("error")
			}
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
