package grange

import (
	"fmt"
	"math"
	"sort"
	"strconv"
)

const END_SYMBOL rune = 4

/* The rule types inferred from the grammar are below. */
type Rule uint8

const (
	RuleUnknown Rule = iota
	Ruleexpression
	Rulerangeexpr
	Rulecombinators
	Ruleintersect
	Ruleexclude
	Ruleunion
	Rulebraces
	Rulesubexpr
	Rulegroupq
	Rulecluster
	Rulegroup
	Rulekey
	Rulelocalkey
	Rulefunction
	Rulefuncargs
	Ruleregex
	Ruleliteral
	Rulevalue
	Rulespace
	Ruleq
	RuleAction0
	Rulenull
	RuleAction1
	RuleAction2
	RuleAction3
	RuleAction4
	RuleAction5
	RuleAction6
	RuleAction7
	RuleAction8
	RuleAction9
	RuleAction10
	RuleAction11
	RuleAction12
	RuleAction13
	RulePegText
	RuleAction14
	RuleAction15
	RuleAction16

	RulePre_
	Rule_In_
	Rule_Suf
)

var Rul3s = [...]string{
	"Unknown",
	"expression",
	"rangeexpr",
	"combinators",
	"intersect",
	"exclude",
	"union",
	"braces",
	"subexpr",
	"groupq",
	"cluster",
	"group",
	"key",
	"localkey",
	"function",
	"funcargs",
	"regex",
	"literal",
	"value",
	"space",
	"q",
	"Action0",
	"null",
	"Action1",
	"Action2",
	"Action3",
	"Action4",
	"Action5",
	"Action6",
	"Action7",
	"Action8",
	"Action9",
	"Action10",
	"Action11",
	"Action12",
	"Action13",
	"PegText",
	"Action14",
	"Action15",
	"Action16",

	"Pre_",
	"_In_",
	"_Suf",
}

type TokenTree interface {
	Print()
	PrintSyntax()
	PrintSyntaxTree(buffer string)
	Add(rule Rule, begin, end, next, depth int)
	Expand(index int) TokenTree
	Tokens() <-chan token32
	AST() *Node32
	Error() []token32
	trim(length int)
}

type Node32 struct {
	token32
	up, next *Node32
}

func (node *Node32) print(depth int, buffer string) {
	for node != nil {
		for c := 0; c < depth; c++ {
			fmt.Printf(" ")
		}
		fmt.Printf("\x1B[34m%v\x1B[m %v\n", Rul3s[node.Rule], strconv.Quote(buffer[node.begin:node.end]))
		if node.up != nil {
			node.up.print(depth+1, buffer)
		}
		node = node.next
	}
}

func (ast *Node32) Print(buffer string) {
	ast.print(0, buffer)
}

type element struct {
	node *Node32
	down *element
}

/* ${@} bit structure for abstract syntax tree */
type token16 struct {
	Rule
	begin, end, next int16
}

func (t *token16) isZero() bool {
	return t.Rule == RuleUnknown && t.begin == 0 && t.end == 0 && t.next == 0
}

func (t *token16) isParentOf(u token16) bool {
	return t.begin <= u.begin && t.end >= u.end && t.next > u.next
}

func (t *token16) GetToken32() token32 {
	return token32{Rule: t.Rule, begin: int32(t.begin), end: int32(t.end), next: int32(t.next)}
}

func (t *token16) String() string {
	return fmt.Sprintf("\x1B[34m%v\x1B[m %v %v %v", Rul3s[t.Rule], t.begin, t.end, t.next)
}

type tokens16 struct {
	tree    []token16
	ordered [][]token16
}

func (t *tokens16) trim(length int) {
	t.tree = t.tree[0:length]
}

func (t *tokens16) Print() {
	for _, token := range t.tree {
		fmt.Println(token.String())
	}
}

func (t *tokens16) Order() [][]token16 {
	if t.ordered != nil {
		return t.ordered
	}

	depths := make([]int16, 1, math.MaxInt16)
	for i, token := range t.tree {
		if token.Rule == RuleUnknown {
			t.tree = t.tree[:i]
			break
		}
		depth := int(token.next)
		if length := len(depths); depth >= length {
			depths = depths[:depth+1]
		}
		depths[depth]++
	}
	depths = append(depths, 0)

	ordered, pool := make([][]token16, len(depths)), make([]token16, len(t.tree)+len(depths))
	for i, depth := range depths {
		depth++
		ordered[i], pool, depths[i] = pool[:depth], pool[depth:], 0
	}

	for i, token := range t.tree {
		depth := token.next
		token.next = int16(i)
		ordered[depth][depths[depth]] = token
		depths[depth]++
	}
	t.ordered = ordered
	return ordered
}

type State16 struct {
	token16
	depths []int16
	leaf   bool
}

func (t *tokens16) AST() *Node32 {
	tokens := t.Tokens()
	stack := &element{node: &Node32{token32: <-tokens}}
	for token := range tokens {
		if token.begin == token.end {
			continue
		}
		node := &Node32{token32: token}
		for stack != nil && stack.node.begin >= token.begin && stack.node.end <= token.end {
			stack.node.next = node.up
			node.up = stack.node
			stack = stack.down
		}
		stack = &element{node: node, down: stack}
	}
	return stack.node
}

func (t *tokens16) PreOrder() (<-chan State16, [][]token16) {
	s, ordered := make(chan State16, 6), t.Order()
	go func() {
		var states [8]State16
		for i, _ := range states {
			states[i].depths = make([]int16, len(ordered))
		}
		depths, state, depth := make([]int16, len(ordered)), 0, 1
		write := func(t token16, leaf bool) {
			S := states[state]
			state, S.Rule, S.begin, S.end, S.next, S.leaf = (state+1)%8, t.Rule, t.begin, t.end, int16(depth), leaf
			copy(S.depths, depths)
			s <- S
		}

		states[state].token16 = ordered[0][0]
		depths[0]++
		state++
		a, b := ordered[depth-1][depths[depth-1]-1], ordered[depth][depths[depth]]
	depthFirstSearch:
		for {
			for {
				if i := depths[depth]; i > 0 {
					if c, j := ordered[depth][i-1], depths[depth-1]; a.isParentOf(c) &&
						(j < 2 || !ordered[depth-1][j-2].isParentOf(c)) {
						if c.end != b.begin {
							write(token16{Rule: Rule_In_, begin: c.end, end: b.begin}, true)
						}
						break
					}
				}

				if a.begin < b.begin {
					write(token16{Rule: RulePre_, begin: a.begin, end: b.begin}, true)
				}
				break
			}

			next := depth + 1
			if c := ordered[next][depths[next]]; c.Rule != RuleUnknown && b.isParentOf(c) {
				write(b, false)
				depths[depth]++
				depth, a, b = next, b, c
				continue
			}

			write(b, true)
			depths[depth]++
			c, parent := ordered[depth][depths[depth]], true
			for {
				if c.Rule != RuleUnknown && a.isParentOf(c) {
					b = c
					continue depthFirstSearch
				} else if parent && b.end != a.end {
					write(token16{Rule: Rule_Suf, begin: b.end, end: a.end}, true)
				}

				depth--
				if depth > 0 {
					a, b, c = ordered[depth-1][depths[depth-1]-1], a, ordered[depth][depths[depth]]
					parent = a.isParentOf(b)
					continue
				}

				break depthFirstSearch
			}
		}

		close(s)
	}()
	return s, ordered
}

func (t *tokens16) PrintSyntax() {
	tokens, ordered := t.PreOrder()
	max := -1
	for token := range tokens {
		if !token.leaf {
			fmt.Printf("%v", token.begin)
			for i, leaf, depths := 0, int(token.next), token.depths; i < leaf; i++ {
				fmt.Printf(" \x1B[36m%v\x1B[m", Rul3s[ordered[i][depths[i]-1].Rule])
			}
			fmt.Printf(" \x1B[36m%v\x1B[m\n", Rul3s[token.Rule])
		} else if token.begin == token.end {
			fmt.Printf("%v", token.begin)
			for i, leaf, depths := 0, int(token.next), token.depths; i < leaf; i++ {
				fmt.Printf(" \x1B[31m%v\x1B[m", Rul3s[ordered[i][depths[i]-1].Rule])
			}
			fmt.Printf(" \x1B[31m%v\x1B[m\n", Rul3s[token.Rule])
		} else {
			for c, end := token.begin, token.end; c < end; c++ {
				if i := int(c); max+1 < i {
					for j := max; j < i; j++ {
						fmt.Printf("skip %v %v\n", j, token.String())
					}
					max = i
				} else if i := int(c); i <= max {
					for j := i; j <= max; j++ {
						fmt.Printf("dupe %v %v\n", j, token.String())
					}
				} else {
					max = int(c)
				}
				fmt.Printf("%v", c)
				for i, leaf, depths := 0, int(token.next), token.depths; i < leaf; i++ {
					fmt.Printf(" \x1B[34m%v\x1B[m", Rul3s[ordered[i][depths[i]-1].Rule])
				}
				fmt.Printf(" \x1B[34m%v\x1B[m\n", Rul3s[token.Rule])
			}
			fmt.Printf("\n")
		}
	}
}

func (t *tokens16) PrintSyntaxTree(buffer string) {
	tokens, _ := t.PreOrder()
	for token := range tokens {
		for c := 0; c < int(token.next); c++ {
			fmt.Printf(" ")
		}
		fmt.Printf("\x1B[34m%v\x1B[m %v\n", Rul3s[token.Rule], strconv.Quote(buffer[token.begin:token.end]))
	}
}

func (t *tokens16) Add(rule Rule, begin, end, depth, index int) {
	t.tree[index] = token16{Rule: rule, begin: int16(begin), end: int16(end), next: int16(depth)}
}

func (t *tokens16) Tokens() <-chan token32 {
	s := make(chan token32, 16)
	go func() {
		for _, v := range t.tree {
			s <- v.GetToken32()
		}
		close(s)
	}()
	return s
}

func (t *tokens16) Error() []token32 {
	ordered := t.Order()
	length := len(ordered)
	tokens, length := make([]token32, length), length-1
	for i, _ := range tokens {
		o := ordered[length-i]
		if len(o) > 1 {
			tokens[i] = o[len(o)-2].GetToken32()
		}
	}
	return tokens
}

/* ${@} bit structure for abstract syntax tree */
type token32 struct {
	Rule
	begin, end, next int32
}

func (t *token32) isZero() bool {
	return t.Rule == RuleUnknown && t.begin == 0 && t.end == 0 && t.next == 0
}

func (t *token32) isParentOf(u token32) bool {
	return t.begin <= u.begin && t.end >= u.end && t.next > u.next
}

func (t *token32) GetToken32() token32 {
	return token32{Rule: t.Rule, begin: int32(t.begin), end: int32(t.end), next: int32(t.next)}
}

func (t *token32) String() string {
	return fmt.Sprintf("\x1B[34m%v\x1B[m %v %v %v", Rul3s[t.Rule], t.begin, t.end, t.next)
}

type tokens32 struct {
	tree    []token32
	ordered [][]token32
}

func (t *tokens32) trim(length int) {
	t.tree = t.tree[0:length]
}

func (t *tokens32) Print() {
	for _, token := range t.tree {
		fmt.Println(token.String())
	}
}

func (t *tokens32) Order() [][]token32 {
	if t.ordered != nil {
		return t.ordered
	}

	depths := make([]int32, 1, math.MaxInt16)
	for i, token := range t.tree {
		if token.Rule == RuleUnknown {
			t.tree = t.tree[:i]
			break
		}
		depth := int(token.next)
		if length := len(depths); depth >= length {
			depths = depths[:depth+1]
		}
		depths[depth]++
	}
	depths = append(depths, 0)

	ordered, pool := make([][]token32, len(depths)), make([]token32, len(t.tree)+len(depths))
	for i, depth := range depths {
		depth++
		ordered[i], pool, depths[i] = pool[:depth], pool[depth:], 0
	}

	for i, token := range t.tree {
		depth := token.next
		token.next = int32(i)
		ordered[depth][depths[depth]] = token
		depths[depth]++
	}
	t.ordered = ordered
	return ordered
}

type State32 struct {
	token32
	depths []int32
	leaf   bool
}

func (t *tokens32) AST() *Node32 {
	tokens := t.Tokens()
	stack := &element{node: &Node32{token32: <-tokens}}
	for token := range tokens {
		if token.begin == token.end {
			continue
		}
		node := &Node32{token32: token}
		for stack != nil && stack.node.begin >= token.begin && stack.node.end <= token.end {
			stack.node.next = node.up
			node.up = stack.node
			stack = stack.down
		}
		stack = &element{node: node, down: stack}
	}
	return stack.node
}

func (t *tokens32) PreOrder() (<-chan State32, [][]token32) {
	s, ordered := make(chan State32, 6), t.Order()
	go func() {
		var states [8]State32
		for i, _ := range states {
			states[i].depths = make([]int32, len(ordered))
		}
		depths, state, depth := make([]int32, len(ordered)), 0, 1
		write := func(t token32, leaf bool) {
			S := states[state]
			state, S.Rule, S.begin, S.end, S.next, S.leaf = (state+1)%8, t.Rule, t.begin, t.end, int32(depth), leaf
			copy(S.depths, depths)
			s <- S
		}

		states[state].token32 = ordered[0][0]
		depths[0]++
		state++
		a, b := ordered[depth-1][depths[depth-1]-1], ordered[depth][depths[depth]]
	depthFirstSearch:
		for {
			for {
				if i := depths[depth]; i > 0 {
					if c, j := ordered[depth][i-1], depths[depth-1]; a.isParentOf(c) &&
						(j < 2 || !ordered[depth-1][j-2].isParentOf(c)) {
						if c.end != b.begin {
							write(token32{Rule: Rule_In_, begin: c.end, end: b.begin}, true)
						}
						break
					}
				}

				if a.begin < b.begin {
					write(token32{Rule: RulePre_, begin: a.begin, end: b.begin}, true)
				}
				break
			}

			next := depth + 1
			if c := ordered[next][depths[next]]; c.Rule != RuleUnknown && b.isParentOf(c) {
				write(b, false)
				depths[depth]++
				depth, a, b = next, b, c
				continue
			}

			write(b, true)
			depths[depth]++
			c, parent := ordered[depth][depths[depth]], true
			for {
				if c.Rule != RuleUnknown && a.isParentOf(c) {
					b = c
					continue depthFirstSearch
				} else if parent && b.end != a.end {
					write(token32{Rule: Rule_Suf, begin: b.end, end: a.end}, true)
				}

				depth--
				if depth > 0 {
					a, b, c = ordered[depth-1][depths[depth-1]-1], a, ordered[depth][depths[depth]]
					parent = a.isParentOf(b)
					continue
				}

				break depthFirstSearch
			}
		}

		close(s)
	}()
	return s, ordered
}

func (t *tokens32) PrintSyntax() {
	tokens, ordered := t.PreOrder()
	max := -1
	for token := range tokens {
		if !token.leaf {
			fmt.Printf("%v", token.begin)
			for i, leaf, depths := 0, int(token.next), token.depths; i < leaf; i++ {
				fmt.Printf(" \x1B[36m%v\x1B[m", Rul3s[ordered[i][depths[i]-1].Rule])
			}
			fmt.Printf(" \x1B[36m%v\x1B[m\n", Rul3s[token.Rule])
		} else if token.begin == token.end {
			fmt.Printf("%v", token.begin)
			for i, leaf, depths := 0, int(token.next), token.depths; i < leaf; i++ {
				fmt.Printf(" \x1B[31m%v\x1B[m", Rul3s[ordered[i][depths[i]-1].Rule])
			}
			fmt.Printf(" \x1B[31m%v\x1B[m\n", Rul3s[token.Rule])
		} else {
			for c, end := token.begin, token.end; c < end; c++ {
				if i := int(c); max+1 < i {
					for j := max; j < i; j++ {
						fmt.Printf("skip %v %v\n", j, token.String())
					}
					max = i
				} else if i := int(c); i <= max {
					for j := i; j <= max; j++ {
						fmt.Printf("dupe %v %v\n", j, token.String())
					}
				} else {
					max = int(c)
				}
				fmt.Printf("%v", c)
				for i, leaf, depths := 0, int(token.next), token.depths; i < leaf; i++ {
					fmt.Printf(" \x1B[34m%v\x1B[m", Rul3s[ordered[i][depths[i]-1].Rule])
				}
				fmt.Printf(" \x1B[34m%v\x1B[m\n", Rul3s[token.Rule])
			}
			fmt.Printf("\n")
		}
	}
}

func (t *tokens32) PrintSyntaxTree(buffer string) {
	tokens, _ := t.PreOrder()
	for token := range tokens {
		for c := 0; c < int(token.next); c++ {
			fmt.Printf(" ")
		}
		fmt.Printf("\x1B[34m%v\x1B[m %v\n", Rul3s[token.Rule], strconv.Quote(buffer[token.begin:token.end]))
	}
}

func (t *tokens32) Add(rule Rule, begin, end, depth, index int) {
	t.tree[index] = token32{Rule: rule, begin: int32(begin), end: int32(end), next: int32(depth)}
}

func (t *tokens32) Tokens() <-chan token32 {
	s := make(chan token32, 16)
	go func() {
		for _, v := range t.tree {
			s <- v.GetToken32()
		}
		close(s)
	}()
	return s
}

func (t *tokens32) Error() []token32 {
	ordered := t.Order()
	length := len(ordered)
	tokens, length := make([]token32, length), length-1
	for i, _ := range tokens {
		o := ordered[length-i]
		if len(o) > 1 {
			tokens[i] = o[len(o)-2].GetToken32()
		}
	}
	return tokens
}

func (t *tokens16) Expand(index int) TokenTree {
	tree := t.tree
	if index >= len(tree) {
		expanded := make([]token32, 2*len(tree))
		for i, v := range tree {
			expanded[i] = v.GetToken32()
		}
		return &tokens32{tree: expanded}
	}
	return nil
}

func (t *tokens32) Expand(index int) TokenTree {
	tree := t.tree
	if index >= len(tree) {
		expanded := make([]token32, 2*len(tree))
		copy(expanded, tree)
		t.tree = expanded
	}
	return nil
}

type RangeQuery struct {
	currentLiteral string
	nodeStack      []Node

	Buffer string
	buffer []rune
	rules  [40]func() bool
	Parse  func(rule ...int) error
	Reset  func()
	TokenTree
}

type textPosition struct {
	line, symbol int
}

type textPositionMap map[int]textPosition

func translatePositions(buffer string, positions []int) textPositionMap {
	length, translations, j, line, symbol := len(positions), make(textPositionMap, len(positions)), 0, 1, 0
	sort.Ints(positions)

search:
	for i, c := range buffer[0:] {
		if c == '\n' {
			line, symbol = line+1, 0
		} else {
			symbol++
		}
		if i == positions[j] {
			translations[positions[j]] = textPosition{line, symbol}
			for j++; j < length; j++ {
				if i != positions[j] {
					continue search
				}
			}
			break search
		}
	}

	return translations
}

type parseError struct {
	p *RangeQuery
}

func (e *parseError) Error() string {
	tokens, error := e.p.TokenTree.Error(), "\n"
	positions, p := make([]int, 2*len(tokens)), 0
	for _, token := range tokens {
		positions[p], p = int(token.begin), p+1
		positions[p], p = int(token.end), p+1
	}
	translations := translatePositions(e.p.Buffer, positions)
	for _, token := range tokens {
		begin, end := int(token.begin), int(token.end)
		error += fmt.Sprintf("parse error near \x1B[34m%v\x1B[m (line %v symbol %v - line %v symbol %v):\n%v\n",
			Rul3s[token.Rule],
			translations[begin].line, translations[begin].symbol,
			translations[end].line, translations[end].symbol,
			/*strconv.Quote(*/ e.p.Buffer[begin:end] /*)*/)
	}

	return error
}

func (p *RangeQuery) PrintSyntaxTree() {
	p.TokenTree.PrintSyntaxTree(p.Buffer)
}

func (p *RangeQuery) Highlighter() {
	p.TokenTree.PrintSyntax()
}

func (p *RangeQuery) Execute() {
	buffer, begin, end := p.Buffer, 0, 0
	for token := range p.TokenTree.Tokens() {
		switch token.Rule {
		case RulePegText:
			begin, end = int(token.begin), int(token.end)
		case RuleAction0:
			p.AddNull()
		case RuleAction1:
			p.AddOperator(operatorIntersect)
		case RuleAction2:
			p.AddOperator(operatorSubtract)
		case RuleAction3:
			p.AddOperator(operatorUnion)
		case RuleAction4:
			p.AddBraces()
		case RuleAction5:
			p.AddSubexpr()
		case RuleAction6:
			p.AddGroupQuery()
		case RuleAction7:
			p.AddClusterLookup(buffer[begin:end])
		case RuleAction8:
			p.AddGroupLookup(buffer[begin:end])
		case RuleAction9:
			p.AddKeyLookup(buffer[begin:end])
		case RuleAction10:
			p.AddLocalClusterLookup(buffer[begin:end])
		case RuleAction11:
			p.AddFunction(buffer[begin:end])
		case RuleAction12:
			p.AddFuncArg()
		case RuleAction13:
			p.AddFuncArg()
		case RuleAction14:
			p.AddRegex(buffer[begin:end])
		case RuleAction15:
			p.AddValue(buffer[begin:end])
		case RuleAction16:
			p.AddValue(buffer[begin:end])

		}
	}
}

func (p *RangeQuery) Init() {
	p.buffer = []rune(p.Buffer)
	if len(p.buffer) == 0 || p.buffer[len(p.buffer)-1] != END_SYMBOL {
		p.buffer = append(p.buffer, END_SYMBOL)
	}

	var tree TokenTree = &tokens16{tree: make([]token16, math.MaxInt16)}
	position, depth, tokenIndex, buffer, rules := 0, 0, 0, p.buffer, p.rules

	p.Parse = func(rule ...int) error {
		r := 1
		if len(rule) > 0 {
			r = rule[0]
		}
		matches := p.rules[r]()
		p.TokenTree = tree
		if matches {
			p.TokenTree.trim(tokenIndex)
			return nil
		}
		return &parseError{p}
	}

	p.Reset = func() {
		position, tokenIndex, depth = 0, 0, 0
	}

	add := func(rule Rule, begin int) {
		if t := tree.Expand(tokenIndex); t != nil {
			tree = t
		}
		tree.Add(rule, begin, position, depth, tokenIndex)
		tokenIndex++
	}

	matchDot := func() bool {
		if buffer[position] != END_SYMBOL {
			position++
			return true
		}
		return false
	}

	/*matchChar := func(c byte) bool {
		if buffer[position] == c {
			position++
			return true
		}
		return false
	}*/

	/*matchRange := func(lower byte, upper byte) bool {
		if c := buffer[position]; c >= lower && c <= upper {
			position++
			return true
		}
		return false
	}*/

	rules = [...]func() bool{
		nil,
		/* 0 expression <- <(rangeexpr combinators? !.)> */
		func() bool {
			position0, tokenIndex0, depth0 := position, tokenIndex, depth
			{
				position1 := position
				depth++
				if !rules[Rulerangeexpr]() {
					goto l0
				}
				{
					position2, tokenIndex2, depth2 := position, tokenIndex, depth
					if !rules[Rulecombinators]() {
						goto l2
					}
					goto l3
				l2:
					position, tokenIndex, depth = position2, tokenIndex2, depth2
				}
			l3:
				{
					position4, tokenIndex4, depth4 := position, tokenIndex, depth
					if !matchDot() {
						goto l4
					}
					goto l0
				l4:
					position, tokenIndex, depth = position4, tokenIndex4, depth4
				}
				depth--
				add(Ruleexpression, position1)
			}
			return true
		l0:
			position, tokenIndex, depth = position0, tokenIndex0, depth0
			return false
		},
		/* 1 rangeexpr <- <(space (q / function / subexpr / cluster / group / groupq / localkey / regex / value / (Action0 braces) / null))> */
		func() bool {
			position5, tokenIndex5, depth5 := position, tokenIndex, depth
			{
				position6 := position
				depth++
				if !rules[Rulespace]() {
					goto l5
				}
				{
					position7, tokenIndex7, depth7 := position, tokenIndex, depth
					if !rules[Ruleq]() {
						goto l8
					}
					goto l7
				l8:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[Rulefunction]() {
						goto l9
					}
					goto l7
				l9:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[Rulesubexpr]() {
						goto l10
					}
					goto l7
				l10:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[Rulecluster]() {
						goto l11
					}
					goto l7
				l11:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[Rulegroup]() {
						goto l12
					}
					goto l7
				l12:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[Rulegroupq]() {
						goto l13
					}
					goto l7
				l13:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[Rulelocalkey]() {
						goto l14
					}
					goto l7
				l14:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[Ruleregex]() {
						goto l15
					}
					goto l7
				l15:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[Rulevalue]() {
						goto l16
					}
					goto l7
				l16:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[RuleAction0]() {
						goto l17
					}
					if !rules[Rulebraces]() {
						goto l17
					}
					goto l7
				l17:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[Rulenull]() {
						goto l5
					}
				}
			l7:
				depth--
				add(Rulerangeexpr, position6)
			}
			return true
		l5:
			position, tokenIndex, depth = position5, tokenIndex5, depth5
			return false
		},
		/* 2 combinators <- <(space (union / intersect / exclude / braces))> */
		func() bool {
			position18, tokenIndex18, depth18 := position, tokenIndex, depth
			{
				position19 := position
				depth++
				if !rules[Rulespace]() {
					goto l18
				}
				{
					position20, tokenIndex20, depth20 := position, tokenIndex, depth
					if !rules[Ruleunion]() {
						goto l21
					}
					goto l20
				l21:
					position, tokenIndex, depth = position20, tokenIndex20, depth20
					if !rules[Ruleintersect]() {
						goto l22
					}
					goto l20
				l22:
					position, tokenIndex, depth = position20, tokenIndex20, depth20
					if !rules[Ruleexclude]() {
						goto l23
					}
					goto l20
				l23:
					position, tokenIndex, depth = position20, tokenIndex20, depth20
					if !rules[Rulebraces]() {
						goto l18
					}
				}
			l20:
				depth--
				add(Rulecombinators, position19)
			}
			return true
		l18:
			position, tokenIndex, depth = position18, tokenIndex18, depth18
			return false
		},
		/* 3 intersect <- <('&' rangeexpr Action1)> */
		func() bool {
			position24, tokenIndex24, depth24 := position, tokenIndex, depth
			{
				position25 := position
				depth++
				if buffer[position] != rune('&') {
					goto l24
				}
				position++
				if !rules[Rulerangeexpr]() {
					goto l24
				}
				if !rules[RuleAction1]() {
					goto l24
				}
				depth--
				add(Ruleintersect, position25)
			}
			return true
		l24:
			position, tokenIndex, depth = position24, tokenIndex24, depth24
			return false
		},
		/* 4 exclude <- <('-' rangeexpr Action2)> */
		func() bool {
			position26, tokenIndex26, depth26 := position, tokenIndex, depth
			{
				position27 := position
				depth++
				if buffer[position] != rune('-') {
					goto l26
				}
				position++
				if !rules[Rulerangeexpr]() {
					goto l26
				}
				if !rules[RuleAction2]() {
					goto l26
				}
				depth--
				add(Ruleexclude, position27)
			}
			return true
		l26:
			position, tokenIndex, depth = position26, tokenIndex26, depth26
			return false
		},
		/* 5 union <- <(',' rangeexpr Action3)> */
		func() bool {
			position28, tokenIndex28, depth28 := position, tokenIndex, depth
			{
				position29 := position
				depth++
				if buffer[position] != rune(',') {
					goto l28
				}
				position++
				if !rules[Rulerangeexpr]() {
					goto l28
				}
				if !rules[RuleAction3]() {
					goto l28
				}
				depth--
				add(Ruleunion, position29)
			}
			return true
		l28:
			position, tokenIndex, depth = position28, tokenIndex28, depth28
			return false
		},
		/* 6 braces <- <('{' rangeexpr combinators? '}' rangeexpr? Action4)> */
		func() bool {
			position30, tokenIndex30, depth30 := position, tokenIndex, depth
			{
				position31 := position
				depth++
				if buffer[position] != rune('{') {
					goto l30
				}
				position++
				if !rules[Rulerangeexpr]() {
					goto l30
				}
				{
					position32, tokenIndex32, depth32 := position, tokenIndex, depth
					if !rules[Rulecombinators]() {
						goto l32
					}
					goto l33
				l32:
					position, tokenIndex, depth = position32, tokenIndex32, depth32
				}
			l33:
				if buffer[position] != rune('}') {
					goto l30
				}
				position++
				{
					position34, tokenIndex34, depth34 := position, tokenIndex, depth
					if !rules[Rulerangeexpr]() {
						goto l34
					}
					goto l35
				l34:
					position, tokenIndex, depth = position34, tokenIndex34, depth34
				}
			l35:
				if !rules[RuleAction4]() {
					goto l30
				}
				depth--
				add(Rulebraces, position31)
			}
			return true
		l30:
			position, tokenIndex, depth = position30, tokenIndex30, depth30
			return false
		},
		/* 7 subexpr <- <('%' '{' rangeexpr '}' Action5 key?)> */
		func() bool {
			position36, tokenIndex36, depth36 := position, tokenIndex, depth
			{
				position37 := position
				depth++
				if buffer[position] != rune('%') {
					goto l36
				}
				position++
				if buffer[position] != rune('{') {
					goto l36
				}
				position++
				if !rules[Rulerangeexpr]() {
					goto l36
				}
				if buffer[position] != rune('}') {
					goto l36
				}
				position++
				if !rules[RuleAction5]() {
					goto l36
				}
				{
					position38, tokenIndex38, depth38 := position, tokenIndex, depth
					if !rules[Rulekey]() {
						goto l38
					}
					goto l39
				l38:
					position, tokenIndex, depth = position38, tokenIndex38, depth38
				}
			l39:
				depth--
				add(Rulesubexpr, position37)
			}
			return true
		l36:
			position, tokenIndex, depth = position36, tokenIndex36, depth36
			return false
		},
		/* 8 groupq <- <('?' rangeexpr Action6)> */
		func() bool {
			position40, tokenIndex40, depth40 := position, tokenIndex, depth
			{
				position41 := position
				depth++
				if buffer[position] != rune('?') {
					goto l40
				}
				position++
				if !rules[Rulerangeexpr]() {
					goto l40
				}
				if !rules[RuleAction6]() {
					goto l40
				}
				depth--
				add(Rulegroupq, position41)
			}
			return true
		l40:
			position, tokenIndex, depth = position40, tokenIndex40, depth40
			return false
		},
		/* 9 cluster <- <('%' literal Action7 key?)> */
		func() bool {
			position42, tokenIndex42, depth42 := position, tokenIndex, depth
			{
				position43 := position
				depth++
				if buffer[position] != rune('%') {
					goto l42
				}
				position++
				if !rules[Ruleliteral]() {
					goto l42
				}
				if !rules[RuleAction7]() {
					goto l42
				}
				{
					position44, tokenIndex44, depth44 := position, tokenIndex, depth
					if !rules[Rulekey]() {
						goto l44
					}
					goto l45
				l44:
					position, tokenIndex, depth = position44, tokenIndex44, depth44
				}
			l45:
				depth--
				add(Rulecluster, position43)
			}
			return true
		l42:
			position, tokenIndex, depth = position42, tokenIndex42, depth42
			return false
		},
		/* 10 group <- <('@' literal Action8)> */
		func() bool {
			position46, tokenIndex46, depth46 := position, tokenIndex, depth
			{
				position47 := position
				depth++
				if buffer[position] != rune('@') {
					goto l46
				}
				position++
				if !rules[Ruleliteral]() {
					goto l46
				}
				if !rules[RuleAction8]() {
					goto l46
				}
				depth--
				add(Rulegroup, position47)
			}
			return true
		l46:
			position, tokenIndex, depth = position46, tokenIndex46, depth46
			return false
		},
		/* 11 key <- <(':' literal Action9)> */
		func() bool {
			position48, tokenIndex48, depth48 := position, tokenIndex, depth
			{
				position49 := position
				depth++
				if buffer[position] != rune(':') {
					goto l48
				}
				position++
				if !rules[Ruleliteral]() {
					goto l48
				}
				if !rules[RuleAction9]() {
					goto l48
				}
				depth--
				add(Rulekey, position49)
			}
			return true
		l48:
			position, tokenIndex, depth = position48, tokenIndex48, depth48
			return false
		},
		/* 12 localkey <- <('$' literal Action10)> */
		func() bool {
			position50, tokenIndex50, depth50 := position, tokenIndex, depth
			{
				position51 := position
				depth++
				if buffer[position] != rune('$') {
					goto l50
				}
				position++
				if !rules[Ruleliteral]() {
					goto l50
				}
				if !rules[RuleAction10]() {
					goto l50
				}
				depth--
				add(Rulelocalkey, position51)
			}
			return true
		l50:
			position, tokenIndex, depth = position50, tokenIndex50, depth50
			return false
		},
		/* 13 function <- <(literal Action11 '(' funcargs ')')> */
		func() bool {
			position52, tokenIndex52, depth52 := position, tokenIndex, depth
			{
				position53 := position
				depth++
				if !rules[Ruleliteral]() {
					goto l52
				}
				if !rules[RuleAction11]() {
					goto l52
				}
				if buffer[position] != rune('(') {
					goto l52
				}
				position++
				if !rules[Rulefuncargs]() {
					goto l52
				}
				if buffer[position] != rune(')') {
					goto l52
				}
				position++
				depth--
				add(Rulefunction, position53)
			}
			return true
		l52:
			position, tokenIndex, depth = position52, tokenIndex52, depth52
			return false
		},
		/* 14 funcargs <- <((rangeexpr Action12 ';' funcargs) / (rangeexpr Action13))> */
		func() bool {
			position54, tokenIndex54, depth54 := position, tokenIndex, depth
			{
				position55 := position
				depth++
				{
					position56, tokenIndex56, depth56 := position, tokenIndex, depth
					if !rules[Rulerangeexpr]() {
						goto l57
					}
					if !rules[RuleAction12]() {
						goto l57
					}
					if buffer[position] != rune(';') {
						goto l57
					}
					position++
					if !rules[Rulefuncargs]() {
						goto l57
					}
					goto l56
				l57:
					position, tokenIndex, depth = position56, tokenIndex56, depth56
					if !rules[Rulerangeexpr]() {
						goto l54
					}
					if !rules[RuleAction13]() {
						goto l54
					}
				}
			l56:
				depth--
				add(Rulefuncargs, position55)
			}
			return true
		l54:
			position, tokenIndex, depth = position54, tokenIndex54, depth54
			return false
		},
		/* 15 regex <- <('/' <(!'/' .)*> '/' Action14)> */
		func() bool {
			position58, tokenIndex58, depth58 := position, tokenIndex, depth
			{
				position59 := position
				depth++
				if buffer[position] != rune('/') {
					goto l58
				}
				position++
				{
					position60 := position
					depth++
				l61:
					{
						position62, tokenIndex62, depth62 := position, tokenIndex, depth
						{
							position63, tokenIndex63, depth63 := position, tokenIndex, depth
							if buffer[position] != rune('/') {
								goto l63
							}
							position++
							goto l62
						l63:
							position, tokenIndex, depth = position63, tokenIndex63, depth63
						}
						if !matchDot() {
							goto l62
						}
						goto l61
					l62:
						position, tokenIndex, depth = position62, tokenIndex62, depth62
					}
					depth--
					add(RulePegText, position60)
				}
				if buffer[position] != rune('/') {
					goto l58
				}
				position++
				if !rules[RuleAction14]() {
					goto l58
				}
				depth--
				add(Ruleregex, position59)
			}
			return true
		l58:
			position, tokenIndex, depth = position58, tokenIndex58, depth58
			return false
		},
		/* 16 literal <- <<([a-z] / [A-Z] / ([0-9] / [0-9]) / '-' / '_')+>> */
		func() bool {
			position64, tokenIndex64, depth64 := position, tokenIndex, depth
			{
				position65 := position
				depth++
				{
					position66 := position
					depth++
					{
						position69, tokenIndex69, depth69 := position, tokenIndex, depth
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l70
						}
						position++
						goto l69
					l70:
						position, tokenIndex, depth = position69, tokenIndex69, depth69
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l71
						}
						position++
						goto l69
					l71:
						position, tokenIndex, depth = position69, tokenIndex69, depth69
						{
							position73, tokenIndex73, depth73 := position, tokenIndex, depth
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l74
							}
							position++
							goto l73
						l74:
							position, tokenIndex, depth = position73, tokenIndex73, depth73
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l72
							}
							position++
						}
					l73:
						goto l69
					l72:
						position, tokenIndex, depth = position69, tokenIndex69, depth69
						if buffer[position] != rune('-') {
							goto l75
						}
						position++
						goto l69
					l75:
						position, tokenIndex, depth = position69, tokenIndex69, depth69
						if buffer[position] != rune('_') {
							goto l64
						}
						position++
					}
				l69:
				l67:
					{
						position68, tokenIndex68, depth68 := position, tokenIndex, depth
						{
							position76, tokenIndex76, depth76 := position, tokenIndex, depth
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l77
							}
							position++
							goto l76
						l77:
							position, tokenIndex, depth = position76, tokenIndex76, depth76
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l78
							}
							position++
							goto l76
						l78:
							position, tokenIndex, depth = position76, tokenIndex76, depth76
							{
								position80, tokenIndex80, depth80 := position, tokenIndex, depth
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l81
								}
								position++
								goto l80
							l81:
								position, tokenIndex, depth = position80, tokenIndex80, depth80
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l79
								}
								position++
							}
						l80:
							goto l76
						l79:
							position, tokenIndex, depth = position76, tokenIndex76, depth76
							if buffer[position] != rune('-') {
								goto l82
							}
							position++
							goto l76
						l82:
							position, tokenIndex, depth = position76, tokenIndex76, depth76
							if buffer[position] != rune('_') {
								goto l68
							}
							position++
						}
					l76:
						goto l67
					l68:
						position, tokenIndex, depth = position68, tokenIndex68, depth68
					}
					depth--
					add(RulePegText, position66)
				}
				depth--
				add(Ruleliteral, position65)
			}
			return true
		l64:
			position, tokenIndex, depth = position64, tokenIndex64, depth64
			return false
		},
		/* 17 value <- <(<([a-z] / [A-Z] / ([0-9] / [0-9]) / '-' / '_' / '.')+> Action15)> */
		func() bool {
			position83, tokenIndex83, depth83 := position, tokenIndex, depth
			{
				position84 := position
				depth++
				{
					position85 := position
					depth++
					{
						position88, tokenIndex88, depth88 := position, tokenIndex, depth
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l89
						}
						position++
						goto l88
					l89:
						position, tokenIndex, depth = position88, tokenIndex88, depth88
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l90
						}
						position++
						goto l88
					l90:
						position, tokenIndex, depth = position88, tokenIndex88, depth88
						{
							position92, tokenIndex92, depth92 := position, tokenIndex, depth
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l93
							}
							position++
							goto l92
						l93:
							position, tokenIndex, depth = position92, tokenIndex92, depth92
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l91
							}
							position++
						}
					l92:
						goto l88
					l91:
						position, tokenIndex, depth = position88, tokenIndex88, depth88
						if buffer[position] != rune('-') {
							goto l94
						}
						position++
						goto l88
					l94:
						position, tokenIndex, depth = position88, tokenIndex88, depth88
						if buffer[position] != rune('_') {
							goto l95
						}
						position++
						goto l88
					l95:
						position, tokenIndex, depth = position88, tokenIndex88, depth88
						if buffer[position] != rune('.') {
							goto l83
						}
						position++
					}
				l88:
				l86:
					{
						position87, tokenIndex87, depth87 := position, tokenIndex, depth
						{
							position96, tokenIndex96, depth96 := position, tokenIndex, depth
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l97
							}
							position++
							goto l96
						l97:
							position, tokenIndex, depth = position96, tokenIndex96, depth96
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l98
							}
							position++
							goto l96
						l98:
							position, tokenIndex, depth = position96, tokenIndex96, depth96
							{
								position100, tokenIndex100, depth100 := position, tokenIndex, depth
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l101
								}
								position++
								goto l100
							l101:
								position, tokenIndex, depth = position100, tokenIndex100, depth100
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l99
								}
								position++
							}
						l100:
							goto l96
						l99:
							position, tokenIndex, depth = position96, tokenIndex96, depth96
							if buffer[position] != rune('-') {
								goto l102
							}
							position++
							goto l96
						l102:
							position, tokenIndex, depth = position96, tokenIndex96, depth96
							if buffer[position] != rune('_') {
								goto l103
							}
							position++
							goto l96
						l103:
							position, tokenIndex, depth = position96, tokenIndex96, depth96
							if buffer[position] != rune('.') {
								goto l87
							}
							position++
						}
					l96:
						goto l86
					l87:
						position, tokenIndex, depth = position87, tokenIndex87, depth87
					}
					depth--
					add(RulePegText, position85)
				}
				if !rules[RuleAction15]() {
					goto l83
				}
				depth--
				add(Rulevalue, position84)
			}
			return true
		l83:
			position, tokenIndex, depth = position83, tokenIndex83, depth83
			return false
		},
		/* 18 space <- <' '*> */
		func() bool {
			{
				position105 := position
				depth++
			l106:
				{
					position107, tokenIndex107, depth107 := position, tokenIndex, depth
					if buffer[position] != rune(' ') {
						goto l107
					}
					position++
					goto l106
				l107:
					position, tokenIndex, depth = position107, tokenIndex107, depth107
				}
				depth--
				add(Rulespace, position105)
			}
			return true
		},
		/* 19 q <- <('q' '(' <(!')' .)*> ')' Action16)> */
		func() bool {
			position108, tokenIndex108, depth108 := position, tokenIndex, depth
			{
				position109 := position
				depth++
				if buffer[position] != rune('q') {
					goto l108
				}
				position++
				if buffer[position] != rune('(') {
					goto l108
				}
				position++
				{
					position110 := position
					depth++
				l111:
					{
						position112, tokenIndex112, depth112 := position, tokenIndex, depth
						{
							position113, tokenIndex113, depth113 := position, tokenIndex, depth
							if buffer[position] != rune(')') {
								goto l113
							}
							position++
							goto l112
						l113:
							position, tokenIndex, depth = position113, tokenIndex113, depth113
						}
						if !matchDot() {
							goto l112
						}
						goto l111
					l112:
						position, tokenIndex, depth = position112, tokenIndex112, depth112
					}
					depth--
					add(RulePegText, position110)
				}
				if buffer[position] != rune(')') {
					goto l108
				}
				position++
				if !rules[RuleAction16]() {
					goto l108
				}
				depth--
				add(Ruleq, position109)
			}
			return true
		l108:
			position, tokenIndex, depth = position108, tokenIndex108, depth108
			return false
		},
		/* 21 Action0 <- <{ p.AddNull() }> */
		func() bool {
			{
				add(RuleAction0, position)
			}
			return true
		},
		/* 22 null <- <> */
		func() bool {
			{
				position117 := position
				depth++
				depth--
				add(Rulenull, position117)
			}
			return true
		},
		/* 23 Action1 <- <{ p.AddOperator(operatorIntersect) }> */
		func() bool {
			{
				add(RuleAction1, position)
			}
			return true
		},
		/* 24 Action2 <- <{ p.AddOperator(operatorSubtract) }> */
		func() bool {
			{
				add(RuleAction2, position)
			}
			return true
		},
		/* 25 Action3 <- <{ p.AddOperator(operatorUnion) }> */
		func() bool {
			{
				add(RuleAction3, position)
			}
			return true
		},
		/* 26 Action4 <- <{ p.AddBraces() }> */
		func() bool {
			{
				add(RuleAction4, position)
			}
			return true
		},
		/* 27 Action5 <- <{ p.AddSubexpr() }> */
		func() bool {
			{
				add(RuleAction5, position)
			}
			return true
		},
		/* 28 Action6 <- <{ p.AddGroupQuery() }> */
		func() bool {
			{
				add(RuleAction6, position)
			}
			return true
		},
		/* 29 Action7 <- <{ p.AddClusterLookup(buffer[begin:end]) }> */
		func() bool {
			{
				add(RuleAction7, position)
			}
			return true
		},
		/* 30 Action8 <- <{ p.AddGroupLookup(buffer[begin:end]) }> */
		func() bool {
			{
				add(RuleAction8, position)
			}
			return true
		},
		/* 31 Action9 <- <{ p.AddKeyLookup(buffer[begin:end]) }> */
		func() bool {
			{
				add(RuleAction9, position)
			}
			return true
		},
		/* 32 Action10 <- <{ p.AddLocalClusterLookup(buffer[begin:end]) }> */
		func() bool {
			{
				add(RuleAction10, position)
			}
			return true
		},
		/* 33 Action11 <- <{ p.AddFunction(buffer[begin:end]) }> */
		func() bool {
			{
				add(RuleAction11, position)
			}
			return true
		},
		/* 34 Action12 <- <{ p.AddFuncArg() }> */
		func() bool {
			{
				add(RuleAction12, position)
			}
			return true
		},
		/* 35 Action13 <- <{ p.AddFuncArg() }> */
		func() bool {
			{
				add(RuleAction13, position)
			}
			return true
		},
		nil,
		/* 37 Action14 <- <{ p.AddRegex(buffer[begin:end]) }> */
		func() bool {
			{
				add(RuleAction14, position)
			}
			return true
		},
		/* 38 Action15 <- <{ p.AddValue(buffer[begin:end]) }> */
		func() bool {
			{
				add(RuleAction15, position)
			}
			return true
		},
		/* 39 Action16 <- <{ p.AddValue(buffer[begin:end]) }> */
		func() bool {
			{
				add(RuleAction16, position)
			}
			return true
		},
	}
	p.rules = rules
}
