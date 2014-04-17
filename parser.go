package grange

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
	right := r.popNode()
	node := r.popNode()
	var left Node
	if len(r.nodeStack) == 0 {
		left = NullNode{}
	} else {
		left = r.popNode()
	}
	r.pushNode(BracesNode{node, left, right})
}

func (r *RangeQuery) AddClusterLookup(name string) {
	r.pushNode(ClusterLookupNode{name, "CLUSTER"})
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

func (r *RangeQuery) AddSubexpr() {
	exprNode := r.popNode()
	r.pushNode(SubexprNode{exprNode, "CLUSTER"})
}

func (r *RangeQuery) AddRegex(val string) {
	r.pushNode(RegexNode{val})
}

func (r *RangeQuery) AddKeyLookup(key string) {
	node := r.popNode()
	switch node.(type) {
	case ClusterLookupNode:
		n := node.(ClusterLookupNode)
		n.key = key
		r.pushNode(n)
	case SubexprNode:
		n := node.(SubexprNode)
		n.key = key
		r.pushNode(n)
	}
}

func (r *RangeQuery) AddOperator(typ operatorType) {
	right := r.popNode()
	left := r.popNode()

	r.pushNode(OperatorNode{typ, left, right})
}
