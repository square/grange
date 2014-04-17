package grange

import (
	"fmt"
	"strings"
)

type operatorType int

const (
	operatorIntersect operatorType = iota
	operatorSubtract
	operatorUnion
)

type Node interface {
	String() string
}

type NullNode struct{}

type TextNode struct {
	val string
}

type RegexNode struct {
	val string
}

type LocalClusterLookupNode struct {
	key string
}

type GroupLookupNode struct {
	node Node
}

type GroupQueryNode struct {
	node Node
}

type ClusterLookupNode struct {
	node Node
	key  string
}

type OperatorNode struct {
	op    operatorType
	left  Node
	right Node
}

type BracesNode struct {
	node  Node
	left  Node
	right Node
}

type FunctionNode struct {
	name   string
	params []Node
}

func (n FunctionNode) String() string {
	result := []string{}
	for _, param := range n.params {
		result = append(result, param.String())
	}

	return fmt.Sprintf("%s(%s)", n.name, strings.Join(result, ";"))
}

func (n TextNode) String() string {
	return n.val
}

func (n RegexNode) String() string {
	return fmt.Sprintf("/%s/", n.val)
}

func (n ClusterLookupNode) String() string {
	if n.key == "CLUSTER" {
		return fmt.Sprintf("%%{%s}", n.node)
	} else {
		return fmt.Sprintf("%%{%s}:%s", n.node, n.key)
	}
}

func (n GroupLookupNode) String() string {
	return fmt.Sprintf("@%s", n.node)
}

func (n GroupQueryNode) String() string {
	return fmt.Sprintf("?%s", n.node)
}

func (n LocalClusterLookupNode) String() string {
	return fmt.Sprintf("$%s", n.key)
}

func (n BracesNode) String() string {
	return fmt.Sprintf("%s{%s}%s", n.node, n.left, n.right)
}

func (n NullNode) String() string {
	return ""
}

func (n OperatorNode) String() string {
	var op string

	switch n.op {
	case operatorIntersect:
		op = "&"
	case operatorSubtract:
		op = "-"
	case operatorUnion:
		op = ","
	}
	return fmt.Sprintf("%s %s %s", n.left, op, n.right)
}
