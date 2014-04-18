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

func (r *RangeQuery) AddConstant(val string) {
	r.pushNode(ConstantNode{val})
}

func (r *RangeQuery) AddNull() {
	r.pushNode(NullNode{})
}

func (r *RangeQuery) AddBraceStart() {
	r.pushNode(BraceStartNode{})
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
	left = NullNode{}

	// This is kind of bullshit but not sure a better way to do it yet
	switch node.(type) {
	case BraceStartNode:
		node = NullNode{}
	default:
		if len(r.nodeStack) > 0 {
			left = r.popNode()
			switch left.(type) {
			case BraceStartNode:
				left = NullNode{}
			}
		}
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
	exprNode := r.popNode()
	r.pushNode(ClusterLookupNode{exprNode, ConstantNode{"CLUSTER"}})
}

func (r *RangeQuery) AddRegex(val string) {
	r.pushNode(RegexNode{val})
}

func (r *RangeQuery) AddKeyLookup() {
	keyNode := r.popNode()
	// TODO: Error out if no lookup
	if len(r.nodeStack) > 0 {
		lookupNode := r.popNode()

		switch lookupNode.(type) {
		case ClusterLookupNode:
			n := lookupNode.(ClusterLookupNode)
			n.key = keyNode
			r.pushNode(n)
			// TODO: Error out if wrong node type
		}
	}
}

func (r *RangeQuery) AddOperator(typ operatorType) {
	right := r.popNode()
	left := r.popNode()

	r.pushNode(OperatorNode{typ, left, right})
}
