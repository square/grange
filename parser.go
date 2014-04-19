package grange

func (r *rangeQuery) popNode() Node {
	l := len(r.nodeStack)
	result := r.nodeStack[l-1]
	r.nodeStack = r.nodeStack[:l-1]
	return result
}

func (r *rangeQuery) pushNode(node Node) {
	r.nodeStack = append(r.nodeStack, node)
}

func (r *rangeQuery) AddValue(val string) {
	r.pushNode(TextNode{val})
}

func (r *rangeQuery) AddConstant(val string) {
	r.pushNode(ConstantNode{val})
}

func (r *rangeQuery) AddNull() {
	r.pushNode(NullNode{})
}

func (r *rangeQuery) AddBraceStart() {
	r.pushNode(BraceStartNode{})
}

func (r *rangeQuery) AddFuncArg() {
	var funcNode Node

	paramNode := r.popNode()
	funcNode = r.nodeStack[len(r.nodeStack)-1]
	fn := funcNode.(FunctionNode)
	fn.params = append(fn.params, paramNode)
	r.nodeStack[len(r.nodeStack)-1] = fn
}

func (r *rangeQuery) AddBraces() {
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

func (r *rangeQuery) AddGroupLookup() {
	exprNode := r.popNode()
	r.pushNode(GroupLookupNode{exprNode})
}

func (r *rangeQuery) AddGroupQuery() {
	exprNode := r.popNode()
	r.pushNode(GroupQueryNode{exprNode})
}

func (r *rangeQuery) AddLocalClusterLookup(key string) {
	r.pushNode(LocalClusterLookupNode{key})
}

func (r *rangeQuery) AddFunction(name string) {
	r.pushNode(FunctionNode{name, []Node{}})
}

func (r *rangeQuery) AddClusterLookup() {
	exprNode := r.popNode()
	r.pushNode(ClusterLookupNode{exprNode, ConstantNode{"CLUSTER"}})
}

func (r *rangeQuery) AddRegex(val string) {
	r.pushNode(RegexNode{val})
}

func (r *rangeQuery) AddKeyLookup() {
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

func (r *rangeQuery) AddOperator(typ operatorType) {
	right := r.popNode()
	left := r.popNode()

	r.pushNode(OperatorNode{typ, left, right})
}
