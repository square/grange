package grange
import "fmt"

func (r *RangeQuery) popNode() Node {
	l := len(r.nodeStack)
	result := r.nodeStack[l-1]
	r.nodeStack = r.nodeStack[:l-1]
	return result
}

func (r *RangeQuery) pushNode(node Node) {
	r.nodeStack = append(r.nodeStack, node)
}

func (r *RangeQuery) AddValue(val string) {
	r.pushNode(TextNode{val})
}

func (r *RangeQuery) AddConstant(val string) {
	r.pushNode(ConstantNode{val})
}

func (r *RangeQuery) AddNull() {
	r.pushNode(NullNode{})
}

func (r *RangeQuery) AddFuncArg() {
	var funcNode Node

	paramNode := r.popNode()
	funcNode = r.nodeStack[len(r.nodeStack)-1]
	fn := funcNode.(FunctionNode)
	fn.params = append(fn.params, paramNode)
	r.nodeStack[len(r.nodeStack)-1] = fn
}

func (r *RangeQuery) AddBraces() {
  fmt.Printf("Adding braces: %s\n", r.nodeStack)
	right := r.popNode()
  fmt.Printf("Adding braces: %s\n", r.nodeStack)
	node := r.popNode()
	var left Node
	if len(r.nodeStack) == 0 {
		left = NullNode{}
	} else {
		left = r.popNode()
	}
	r.pushNode(BracesNode{node, left, right})
}

func (r *RangeQuery) AddGroupLookup() {
	exprNode := r.popNode()
	r.pushNode(GroupLookupNode{exprNode})
}

func (r *RangeQuery) AddGroupQuery() {
	exprNode := r.popNode()
	r.pushNode(GroupQueryNode{exprNode})
}

func (r *RangeQuery) AddLocalClusterLookup(key string) {
	r.pushNode(LocalClusterLookupNode{key})
}

func (r *RangeQuery) AddFunction(name string) {
	r.pushNode(FunctionNode{name, []Node{}})
}

func (r *RangeQuery) AddClusterLookup() {
  fmt.Printf("%d: %s\n", len(r.nodeStack), r.nodeStack)
	exprNode := r.popNode()
	r.pushNode(ClusterLookupNode{exprNode, TextNode{"CLUSTER"}})
}

func (r *RangeQuery) AddRegex(val string) {
	r.pushNode(RegexNode{val})
}

func (r *RangeQuery) AddKeyLookup() {
  fmt.Printf("%s\n", r.nodeStack)
	exprNode := r.popNode()
  fmt.Printf("%s\n", r.nodeStack)
  node := r.popNode()
	switch node.(type) {
	case ClusterLookupNode:
		n := node.(ClusterLookupNode)
		n.key = exprNode
		r.pushNode(n)
  default:
    // TODO: FAIL?
	}
}

func (r *RangeQuery) AddOperator(typ operatorType) {
	right := r.popNode()
	left := r.popNode()

	r.pushNode(OperatorNode{typ, left, right})
}
