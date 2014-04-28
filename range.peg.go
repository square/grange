package grange

import (
	"fmt"
	"math"
	"sort"
	"strconv"
)

const end_symbol rune = 4

/* The rule types inferred from the grammar are below. */
type pegRule uint8

const (
	ruleUnknown pegRule = iota
	ruleexpression
	rulerangeexpr
	rulecombinators
	ruleintersect
	ruleexclude
	ruleunion
	rulebraces
	rulegroupq
	rulecluster
	rulegroup
	rulekey
	rulelocalkey
	rulefunction
	rulefuncargs
	ruleregex
	ruleliteral
	rulevalue
	ruleleaderChar
	rulespace
	ruleq
	ruleAction0
	rulenull
	ruleAction1
	ruleAction2
	ruleAction3
	ruleAction4
	ruleAction5
	ruleAction6
	ruleAction7
	ruleAction8
	ruleAction9
	ruleAction10
	ruleAction11
	ruleAction12
	rulePegText
	ruleAction13
	ruleAction14
	ruleAction15

	rulePre_
	rule_In_
	rule_Suf
)

var rul3s = [...]string{
	"Unknown",
	"expression",
	"rangeexpr",
	"combinators",
	"intersect",
	"exclude",
	"union",
	"braces",
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
	"leaderChar",
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
	"PegText",
	"Action13",
	"Action14",
	"Action15",

	"Pre_",
	"_In_",
	"_Suf",
}

type tokenTree interface {
	Print()
	PrintSyntax()
	PrintSyntaxTree(buffer string)
	Add(rule pegRule, begin, end, next, depth int)
	Expand(index int) tokenTree
	Tokens() <-chan token32
	AST() *node32
	Error() []token32
	trim(length int)
}

type node32 struct {
	token32
	up, next *node32
}

func (node *node32) print(depth int, buffer string) {
	for node != nil {
		for c := 0; c < depth; c++ {
			fmt.Printf(" ")
		}
		fmt.Printf("\x1B[34m%v\x1B[m %v\n", rul3s[node.pegRule], strconv.Quote(buffer[node.begin:node.end]))
		if node.up != nil {
			node.up.print(depth+1, buffer)
		}
		node = node.next
	}
}

func (ast *node32) Print(buffer string) {
	ast.print(0, buffer)
}

type element struct {
	node *node32
	down *element
}

/* ${@} bit structure for abstract syntax tree */
type token16 struct {
	pegRule
	begin, end, next int16
}

func (t *token16) isZero() bool {
	return t.pegRule == ruleUnknown && t.begin == 0 && t.end == 0 && t.next == 0
}

func (t *token16) isParentOf(u token16) bool {
	return t.begin <= u.begin && t.end >= u.end && t.next > u.next
}

func (t *token16) getToken32() token32 {
	return token32{pegRule: t.pegRule, begin: int32(t.begin), end: int32(t.end), next: int32(t.next)}
}

func (t *token16) String() string {
	return fmt.Sprintf("\x1B[34m%v\x1B[m %v %v %v", rul3s[t.pegRule], t.begin, t.end, t.next)
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
		if token.pegRule == ruleUnknown {
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

type state16 struct {
	token16
	depths []int16
	leaf   bool
}

func (t *tokens16) AST() *node32 {
	tokens := t.Tokens()
	stack := &element{node: &node32{token32: <-tokens}}
	for token := range tokens {
		if token.begin == token.end {
			continue
		}
		node := &node32{token32: token}
		for stack != nil && stack.node.begin >= token.begin && stack.node.end <= token.end {
			stack.node.next = node.up
			node.up = stack.node
			stack = stack.down
		}
		stack = &element{node: node, down: stack}
	}
	return stack.node
}

func (t *tokens16) PreOrder() (<-chan state16, [][]token16) {
	s, ordered := make(chan state16, 6), t.Order()
	go func() {
		var states [8]state16
		for i, _ := range states {
			states[i].depths = make([]int16, len(ordered))
		}
		depths, state, depth := make([]int16, len(ordered)), 0, 1
		write := func(t token16, leaf bool) {
			S := states[state]
			state, S.pegRule, S.begin, S.end, S.next, S.leaf = (state+1)%8, t.pegRule, t.begin, t.end, int16(depth), leaf
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
							write(token16{pegRule: rule_In_, begin: c.end, end: b.begin}, true)
						}
						break
					}
				}

				if a.begin < b.begin {
					write(token16{pegRule: rulePre_, begin: a.begin, end: b.begin}, true)
				}
				break
			}

			next := depth + 1
			if c := ordered[next][depths[next]]; c.pegRule != ruleUnknown && b.isParentOf(c) {
				write(b, false)
				depths[depth]++
				depth, a, b = next, b, c
				continue
			}

			write(b, true)
			depths[depth]++
			c, parent := ordered[depth][depths[depth]], true
			for {
				if c.pegRule != ruleUnknown && a.isParentOf(c) {
					b = c
					continue depthFirstSearch
				} else if parent && b.end != a.end {
					write(token16{pegRule: rule_Suf, begin: b.end, end: a.end}, true)
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
				fmt.Printf(" \x1B[36m%v\x1B[m", rul3s[ordered[i][depths[i]-1].pegRule])
			}
			fmt.Printf(" \x1B[36m%v\x1B[m\n", rul3s[token.pegRule])
		} else if token.begin == token.end {
			fmt.Printf("%v", token.begin)
			for i, leaf, depths := 0, int(token.next), token.depths; i < leaf; i++ {
				fmt.Printf(" \x1B[31m%v\x1B[m", rul3s[ordered[i][depths[i]-1].pegRule])
			}
			fmt.Printf(" \x1B[31m%v\x1B[m\n", rul3s[token.pegRule])
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
					fmt.Printf(" \x1B[34m%v\x1B[m", rul3s[ordered[i][depths[i]-1].pegRule])
				}
				fmt.Printf(" \x1B[34m%v\x1B[m\n", rul3s[token.pegRule])
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
		fmt.Printf("\x1B[34m%v\x1B[m %v\n", rul3s[token.pegRule], strconv.Quote(buffer[token.begin:token.end]))
	}
}

func (t *tokens16) Add(rule pegRule, begin, end, depth, index int) {
	t.tree[index] = token16{pegRule: rule, begin: int16(begin), end: int16(end), next: int16(depth)}
}

func (t *tokens16) Tokens() <-chan token32 {
	s := make(chan token32, 16)
	go func() {
		for _, v := range t.tree {
			s <- v.getToken32()
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
			tokens[i] = o[len(o)-2].getToken32()
		}
	}
	return tokens
}

/* ${@} bit structure for abstract syntax tree */
type token32 struct {
	pegRule
	begin, end, next int32
}

func (t *token32) isZero() bool {
	return t.pegRule == ruleUnknown && t.begin == 0 && t.end == 0 && t.next == 0
}

func (t *token32) isParentOf(u token32) bool {
	return t.begin <= u.begin && t.end >= u.end && t.next > u.next
}

func (t *token32) getToken32() token32 {
	return token32{pegRule: t.pegRule, begin: int32(t.begin), end: int32(t.end), next: int32(t.next)}
}

func (t *token32) String() string {
	return fmt.Sprintf("\x1B[34m%v\x1B[m %v %v %v", rul3s[t.pegRule], t.begin, t.end, t.next)
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
		if token.pegRule == ruleUnknown {
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

type state32 struct {
	token32
	depths []int32
	leaf   bool
}

func (t *tokens32) AST() *node32 {
	tokens := t.Tokens()
	stack := &element{node: &node32{token32: <-tokens}}
	for token := range tokens {
		if token.begin == token.end {
			continue
		}
		node := &node32{token32: token}
		for stack != nil && stack.node.begin >= token.begin && stack.node.end <= token.end {
			stack.node.next = node.up
			node.up = stack.node
			stack = stack.down
		}
		stack = &element{node: node, down: stack}
	}
	return stack.node
}

func (t *tokens32) PreOrder() (<-chan state32, [][]token32) {
	s, ordered := make(chan state32, 6), t.Order()
	go func() {
		var states [8]state32
		for i, _ := range states {
			states[i].depths = make([]int32, len(ordered))
		}
		depths, state, depth := make([]int32, len(ordered)), 0, 1
		write := func(t token32, leaf bool) {
			S := states[state]
			state, S.pegRule, S.begin, S.end, S.next, S.leaf = (state+1)%8, t.pegRule, t.begin, t.end, int32(depth), leaf
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
							write(token32{pegRule: rule_In_, begin: c.end, end: b.begin}, true)
						}
						break
					}
				}

				if a.begin < b.begin {
					write(token32{pegRule: rulePre_, begin: a.begin, end: b.begin}, true)
				}
				break
			}

			next := depth + 1
			if c := ordered[next][depths[next]]; c.pegRule != ruleUnknown && b.isParentOf(c) {
				write(b, false)
				depths[depth]++
				depth, a, b = next, b, c
				continue
			}

			write(b, true)
			depths[depth]++
			c, parent := ordered[depth][depths[depth]], true
			for {
				if c.pegRule != ruleUnknown && a.isParentOf(c) {
					b = c
					continue depthFirstSearch
				} else if parent && b.end != a.end {
					write(token32{pegRule: rule_Suf, begin: b.end, end: a.end}, true)
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
				fmt.Printf(" \x1B[36m%v\x1B[m", rul3s[ordered[i][depths[i]-1].pegRule])
			}
			fmt.Printf(" \x1B[36m%v\x1B[m\n", rul3s[token.pegRule])
		} else if token.begin == token.end {
			fmt.Printf("%v", token.begin)
			for i, leaf, depths := 0, int(token.next), token.depths; i < leaf; i++ {
				fmt.Printf(" \x1B[31m%v\x1B[m", rul3s[ordered[i][depths[i]-1].pegRule])
			}
			fmt.Printf(" \x1B[31m%v\x1B[m\n", rul3s[token.pegRule])
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
					fmt.Printf(" \x1B[34m%v\x1B[m", rul3s[ordered[i][depths[i]-1].pegRule])
				}
				fmt.Printf(" \x1B[34m%v\x1B[m\n", rul3s[token.pegRule])
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
		fmt.Printf("\x1B[34m%v\x1B[m %v\n", rul3s[token.pegRule], strconv.Quote(buffer[token.begin:token.end]))
	}
}

func (t *tokens32) Add(rule pegRule, begin, end, depth, index int) {
	t.tree[index] = token32{pegRule: rule, begin: int32(begin), end: int32(end), next: int32(depth)}
}

func (t *tokens32) Tokens() <-chan token32 {
	s := make(chan token32, 16)
	go func() {
		for _, v := range t.tree {
			s <- v.getToken32()
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
			tokens[i] = o[len(o)-2].getToken32()
		}
	}
	return tokens
}

func (t *tokens16) Expand(index int) tokenTree {
	tree := t.tree
	if index >= len(tree) {
		expanded := make([]token32, 2*len(tree))
		for i, v := range tree {
			expanded[i] = v.getToken32()
		}
		return &tokens32{tree: expanded}
	}
	return nil
}

func (t *tokens32) Expand(index int) tokenTree {
	tree := t.tree
	if index >= len(tree) {
		expanded := make([]token32, 2*len(tree))
		copy(expanded, tree)
		t.tree = expanded
	}
	return nil
}

type rangeQuery struct {
	currentLiteral string
	nodeStack      []parserNode

	Buffer string
	buffer []rune
	rules  [39]func() bool
	Parse  func(rule ...int) error
	Reset  func()
	tokenTree
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
	p *rangeQuery
}

func (e *parseError) Error() string {
	tokens, error := e.p.tokenTree.Error(), "\n"
	positions, p := make([]int, 2*len(tokens)), 0
	for _, token := range tokens {
		positions[p], p = int(token.begin), p+1
		positions[p], p = int(token.end), p+1
	}
	translations := translatePositions(e.p.Buffer, positions)
	for _, token := range tokens {
		begin, end := int(token.begin), int(token.end)
		error += fmt.Sprintf("parse error near \x1B[34m%v\x1B[m (line %v symbol %v - line %v symbol %v):\n%v\n",
			rul3s[token.pegRule],
			translations[begin].line, translations[begin].symbol,
			translations[end].line, translations[end].symbol,
			/*strconv.Quote(*/ e.p.Buffer[begin:end] /*)*/)
	}

	return error
}

func (p *rangeQuery) PrintSyntaxTree() {
	p.tokenTree.PrintSyntaxTree(p.Buffer)
}

func (p *rangeQuery) Highlighter() {
	p.tokenTree.PrintSyntax()
}

func (p *rangeQuery) Execute() {
	buffer, begin, end := p.Buffer, 0, 0
	for token := range p.tokenTree.Tokens() {
		switch token.pegRule {
		case rulePegText:
			begin, end = int(token.begin), int(token.end)
		case ruleAction0:
			p.addBraceStart()
		case ruleAction1:
			p.addOperator(operatorIntersect)
		case ruleAction2:
			p.addOperator(operatorSubtract)
		case ruleAction3:
			p.addOperator(operatorUnion)
		case ruleAction4:
			p.addBraces()
		case ruleAction5:
			p.addGroupQuery()
		case ruleAction6:
			p.addClusterLookup()
		case ruleAction7:
			p.addGroupLookup()
		case ruleAction8:
			p.addKeyLookup()
		case ruleAction9:
			p.addLocalClusterLookup(buffer[begin:end])
		case ruleAction10:
			p.addFunction(buffer[begin:end])
		case ruleAction11:
			p.addFuncArg()
		case ruleAction12:
			p.addFuncArg()
		case ruleAction13:
			p.addRegex(buffer[begin:end])
		case ruleAction14:
			p.addValue(buffer[begin:end])
		case ruleAction15:
			p.addConstant(buffer[begin:end])

		}
	}
}

func (p *rangeQuery) Init() {
	p.buffer = []rune(p.Buffer)
	if len(p.buffer) == 0 || p.buffer[len(p.buffer)-1] != end_symbol {
		p.buffer = append(p.buffer, end_symbol)
	}

	var tree tokenTree = &tokens16{tree: make([]token16, math.MaxInt16)}
	position, depth, tokenIndex, buffer, rules := 0, 0, 0, p.buffer, p.rules

	p.Parse = func(rule ...int) error {
		r := 1
		if len(rule) > 0 {
			r = rule[0]
		}
		matches := p.rules[r]()
		p.tokenTree = tree
		if matches {
			p.tokenTree.trim(tokenIndex)
			return nil
		}
		return &parseError{p}
	}

	p.Reset = func() {
		position, tokenIndex, depth = 0, 0, 0
	}

	add := func(rule pegRule, begin int) {
		if t := tree.Expand(tokenIndex); t != nil {
			tree = t
		}
		tree.Add(rule, begin, position, depth, tokenIndex)
		tokenIndex++
	}

	matchDot := func() bool {
		if buffer[position] != end_symbol {
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
				if !rules[rulerangeexpr]() {
					goto l0
				}
				{
					position2, tokenIndex2, depth2 := position, tokenIndex, depth
					if !rules[rulecombinators]() {
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
				add(ruleexpression, position1)
			}
			return true
		l0:
			position, tokenIndex, depth = position0, tokenIndex0, depth0
			return false
		},
		/* 1 rangeexpr <- <(space (q / function / cluster / group / groupq / localkey / regex / value / (Action0 braces) / null))> */
		func() bool {
			position5, tokenIndex5, depth5 := position, tokenIndex, depth
			{
				position6 := position
				depth++
				if !rules[rulespace]() {
					goto l5
				}
				{
					position7, tokenIndex7, depth7 := position, tokenIndex, depth
					if !rules[ruleq]() {
						goto l8
					}
					goto l7
				l8:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[rulefunction]() {
						goto l9
					}
					goto l7
				l9:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[rulecluster]() {
						goto l10
					}
					goto l7
				l10:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[rulegroup]() {
						goto l11
					}
					goto l7
				l11:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[rulegroupq]() {
						goto l12
					}
					goto l7
				l12:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[rulelocalkey]() {
						goto l13
					}
					goto l7
				l13:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[ruleregex]() {
						goto l14
					}
					goto l7
				l14:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[rulevalue]() {
						goto l15
					}
					goto l7
				l15:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[ruleAction0]() {
						goto l16
					}
					if !rules[rulebraces]() {
						goto l16
					}
					goto l7
				l16:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
					if !rules[rulenull]() {
						goto l5
					}
				}
			l7:
				depth--
				add(rulerangeexpr, position6)
			}
			return true
		l5:
			position, tokenIndex, depth = position5, tokenIndex5, depth5
			return false
		},
		/* 2 combinators <- <(space (union / intersect / exclude / braces))> */
		func() bool {
			position17, tokenIndex17, depth17 := position, tokenIndex, depth
			{
				position18 := position
				depth++
				if !rules[rulespace]() {
					goto l17
				}
				{
					position19, tokenIndex19, depth19 := position, tokenIndex, depth
					if !rules[ruleunion]() {
						goto l20
					}
					goto l19
				l20:
					position, tokenIndex, depth = position19, tokenIndex19, depth19
					if !rules[ruleintersect]() {
						goto l21
					}
					goto l19
				l21:
					position, tokenIndex, depth = position19, tokenIndex19, depth19
					if !rules[ruleexclude]() {
						goto l22
					}
					goto l19
				l22:
					position, tokenIndex, depth = position19, tokenIndex19, depth19
					if !rules[rulebraces]() {
						goto l17
					}
				}
			l19:
				depth--
				add(rulecombinators, position18)
			}
			return true
		l17:
			position, tokenIndex, depth = position17, tokenIndex17, depth17
			return false
		},
		/* 3 intersect <- <('&' rangeexpr Action1 combinators?)> */
		func() bool {
			position23, tokenIndex23, depth23 := position, tokenIndex, depth
			{
				position24 := position
				depth++
				if buffer[position] != rune('&') {
					goto l23
				}
				position++
				if !rules[rulerangeexpr]() {
					goto l23
				}
				if !rules[ruleAction1]() {
					goto l23
				}
				{
					position25, tokenIndex25, depth25 := position, tokenIndex, depth
					if !rules[rulecombinators]() {
						goto l25
					}
					goto l26
				l25:
					position, tokenIndex, depth = position25, tokenIndex25, depth25
				}
			l26:
				depth--
				add(ruleintersect, position24)
			}
			return true
		l23:
			position, tokenIndex, depth = position23, tokenIndex23, depth23
			return false
		},
		/* 4 exclude <- <('-' rangeexpr Action2 combinators?)> */
		func() bool {
			position27, tokenIndex27, depth27 := position, tokenIndex, depth
			{
				position28 := position
				depth++
				if buffer[position] != rune('-') {
					goto l27
				}
				position++
				if !rules[rulerangeexpr]() {
					goto l27
				}
				if !rules[ruleAction2]() {
					goto l27
				}
				{
					position29, tokenIndex29, depth29 := position, tokenIndex, depth
					if !rules[rulecombinators]() {
						goto l29
					}
					goto l30
				l29:
					position, tokenIndex, depth = position29, tokenIndex29, depth29
				}
			l30:
				depth--
				add(ruleexclude, position28)
			}
			return true
		l27:
			position, tokenIndex, depth = position27, tokenIndex27, depth27
			return false
		},
		/* 5 union <- <(',' rangeexpr Action3 combinators?)> */
		func() bool {
			position31, tokenIndex31, depth31 := position, tokenIndex, depth
			{
				position32 := position
				depth++
				if buffer[position] != rune(',') {
					goto l31
				}
				position++
				if !rules[rulerangeexpr]() {
					goto l31
				}
				if !rules[ruleAction3]() {
					goto l31
				}
				{
					position33, tokenIndex33, depth33 := position, tokenIndex, depth
					if !rules[rulecombinators]() {
						goto l33
					}
					goto l34
				l33:
					position, tokenIndex, depth = position33, tokenIndex33, depth33
				}
			l34:
				depth--
				add(ruleunion, position32)
			}
			return true
		l31:
			position, tokenIndex, depth = position31, tokenIndex31, depth31
			return false
		},
		/* 6 braces <- <('{' rangeexpr combinators? '}' rangeexpr? Action4)> */
		func() bool {
			position35, tokenIndex35, depth35 := position, tokenIndex, depth
			{
				position36 := position
				depth++
				if buffer[position] != rune('{') {
					goto l35
				}
				position++
				if !rules[rulerangeexpr]() {
					goto l35
				}
				{
					position37, tokenIndex37, depth37 := position, tokenIndex, depth
					if !rules[rulecombinators]() {
						goto l37
					}
					goto l38
				l37:
					position, tokenIndex, depth = position37, tokenIndex37, depth37
				}
			l38:
				if buffer[position] != rune('}') {
					goto l35
				}
				position++
				{
					position39, tokenIndex39, depth39 := position, tokenIndex, depth
					if !rules[rulerangeexpr]() {
						goto l39
					}
					goto l40
				l39:
					position, tokenIndex, depth = position39, tokenIndex39, depth39
				}
			l40:
				if !rules[ruleAction4]() {
					goto l35
				}
				depth--
				add(rulebraces, position36)
			}
			return true
		l35:
			position, tokenIndex, depth = position35, tokenIndex35, depth35
			return false
		},
		/* 7 groupq <- <('?' rangeexpr Action5)> */
		func() bool {
			position41, tokenIndex41, depth41 := position, tokenIndex, depth
			{
				position42 := position
				depth++
				if buffer[position] != rune('?') {
					goto l41
				}
				position++
				if !rules[rulerangeexpr]() {
					goto l41
				}
				if !rules[ruleAction5]() {
					goto l41
				}
				depth--
				add(rulegroupq, position42)
			}
			return true
		l41:
			position, tokenIndex, depth = position41, tokenIndex41, depth41
			return false
		},
		/* 8 cluster <- <('%' rangeexpr Action6 key?)> */
		func() bool {
			position43, tokenIndex43, depth43 := position, tokenIndex, depth
			{
				position44 := position
				depth++
				if buffer[position] != rune('%') {
					goto l43
				}
				position++
				if !rules[rulerangeexpr]() {
					goto l43
				}
				if !rules[ruleAction6]() {
					goto l43
				}
				{
					position45, tokenIndex45, depth45 := position, tokenIndex, depth
					if !rules[rulekey]() {
						goto l45
					}
					goto l46
				l45:
					position, tokenIndex, depth = position45, tokenIndex45, depth45
				}
			l46:
				depth--
				add(rulecluster, position44)
			}
			return true
		l43:
			position, tokenIndex, depth = position43, tokenIndex43, depth43
			return false
		},
		/* 9 group <- <('@' rangeexpr Action7)> */
		func() bool {
			position47, tokenIndex47, depth47 := position, tokenIndex, depth
			{
				position48 := position
				depth++
				if buffer[position] != rune('@') {
					goto l47
				}
				position++
				if !rules[rulerangeexpr]() {
					goto l47
				}
				if !rules[ruleAction7]() {
					goto l47
				}
				depth--
				add(rulegroup, position48)
			}
			return true
		l47:
			position, tokenIndex, depth = position47, tokenIndex47, depth47
			return false
		},
		/* 10 key <- <(':' rangeexpr Action8)> */
		func() bool {
			position49, tokenIndex49, depth49 := position, tokenIndex, depth
			{
				position50 := position
				depth++
				if buffer[position] != rune(':') {
					goto l49
				}
				position++
				if !rules[rulerangeexpr]() {
					goto l49
				}
				if !rules[ruleAction8]() {
					goto l49
				}
				depth--
				add(rulekey, position50)
			}
			return true
		l49:
			position, tokenIndex, depth = position49, tokenIndex49, depth49
			return false
		},
		/* 11 localkey <- <('$' literal Action9)> */
		func() bool {
			position51, tokenIndex51, depth51 := position, tokenIndex, depth
			{
				position52 := position
				depth++
				if buffer[position] != rune('$') {
					goto l51
				}
				position++
				if !rules[ruleliteral]() {
					goto l51
				}
				if !rules[ruleAction9]() {
					goto l51
				}
				depth--
				add(rulelocalkey, position52)
			}
			return true
		l51:
			position, tokenIndex, depth = position51, tokenIndex51, depth51
			return false
		},
		/* 12 function <- <(literal Action10 '(' funcargs ')')> */
		func() bool {
			position53, tokenIndex53, depth53 := position, tokenIndex, depth
			{
				position54 := position
				depth++
				if !rules[ruleliteral]() {
					goto l53
				}
				if !rules[ruleAction10]() {
					goto l53
				}
				if buffer[position] != rune('(') {
					goto l53
				}
				position++
				if !rules[rulefuncargs]() {
					goto l53
				}
				if buffer[position] != rune(')') {
					goto l53
				}
				position++
				depth--
				add(rulefunction, position54)
			}
			return true
		l53:
			position, tokenIndex, depth = position53, tokenIndex53, depth53
			return false
		},
		/* 13 funcargs <- <((rangeexpr Action11 ';' funcargs) / (rangeexpr Action12))> */
		func() bool {
			position55, tokenIndex55, depth55 := position, tokenIndex, depth
			{
				position56 := position
				depth++
				{
					position57, tokenIndex57, depth57 := position, tokenIndex, depth
					if !rules[rulerangeexpr]() {
						goto l58
					}
					if !rules[ruleAction11]() {
						goto l58
					}
					if buffer[position] != rune(';') {
						goto l58
					}
					position++
					if !rules[rulefuncargs]() {
						goto l58
					}
					goto l57
				l58:
					position, tokenIndex, depth = position57, tokenIndex57, depth57
					if !rules[rulerangeexpr]() {
						goto l55
					}
					if !rules[ruleAction12]() {
						goto l55
					}
				}
			l57:
				depth--
				add(rulefuncargs, position56)
			}
			return true
		l55:
			position, tokenIndex, depth = position55, tokenIndex55, depth55
			return false
		},
		/* 14 regex <- <('/' <(!'/' .)*> '/' Action13)> */
		func() bool {
			position59, tokenIndex59, depth59 := position, tokenIndex, depth
			{
				position60 := position
				depth++
				if buffer[position] != rune('/') {
					goto l59
				}
				position++
				{
					position61 := position
					depth++
				l62:
					{
						position63, tokenIndex63, depth63 := position, tokenIndex, depth
						{
							position64, tokenIndex64, depth64 := position, tokenIndex, depth
							if buffer[position] != rune('/') {
								goto l64
							}
							position++
							goto l63
						l64:
							position, tokenIndex, depth = position64, tokenIndex64, depth64
						}
						if !matchDot() {
							goto l63
						}
						goto l62
					l63:
						position, tokenIndex, depth = position63, tokenIndex63, depth63
					}
					depth--
					add(rulePegText, position61)
				}
				if buffer[position] != rune('/') {
					goto l59
				}
				position++
				if !rules[ruleAction13]() {
					goto l59
				}
				depth--
				add(ruleregex, position60)
			}
			return true
		l59:
			position, tokenIndex, depth = position59, tokenIndex59, depth59
			return false
		},
		/* 15 literal <- <<(leaderChar ([a-z] / [A-Z] / ([0-9] / [0-9]) / '-' / '_')*)>> */
		func() bool {
			position65, tokenIndex65, depth65 := position, tokenIndex, depth
			{
				position66 := position
				depth++
				{
					position67 := position
					depth++
					if !rules[ruleleaderChar]() {
						goto l65
					}
				l68:
					{
						position69, tokenIndex69, depth69 := position, tokenIndex, depth
						{
							position70, tokenIndex70, depth70 := position, tokenIndex, depth
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l71
							}
							position++
							goto l70
						l71:
							position, tokenIndex, depth = position70, tokenIndex70, depth70
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l72
							}
							position++
							goto l70
						l72:
							position, tokenIndex, depth = position70, tokenIndex70, depth70
							{
								position74, tokenIndex74, depth74 := position, tokenIndex, depth
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l75
								}
								position++
								goto l74
							l75:
								position, tokenIndex, depth = position74, tokenIndex74, depth74
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l73
								}
								position++
							}
						l74:
							goto l70
						l73:
							position, tokenIndex, depth = position70, tokenIndex70, depth70
							if buffer[position] != rune('-') {
								goto l76
							}
							position++
							goto l70
						l76:
							position, tokenIndex, depth = position70, tokenIndex70, depth70
							if buffer[position] != rune('_') {
								goto l69
							}
							position++
						}
					l70:
						goto l68
					l69:
						position, tokenIndex, depth = position69, tokenIndex69, depth69
					}
					depth--
					add(rulePegText, position67)
				}
				depth--
				add(ruleliteral, position66)
			}
			return true
		l65:
			position, tokenIndex, depth = position65, tokenIndex65, depth65
			return false
		},
		/* 16 value <- <(<(leaderChar ([a-z] / [A-Z] / ([0-9] / [0-9]) / '-' / '_' / '.')*)> Action14)> */
		func() bool {
			position77, tokenIndex77, depth77 := position, tokenIndex, depth
			{
				position78 := position
				depth++
				{
					position79 := position
					depth++
					if !rules[ruleleaderChar]() {
						goto l77
					}
				l80:
					{
						position81, tokenIndex81, depth81 := position, tokenIndex, depth
						{
							position82, tokenIndex82, depth82 := position, tokenIndex, depth
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l83
							}
							position++
							goto l82
						l83:
							position, tokenIndex, depth = position82, tokenIndex82, depth82
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l84
							}
							position++
							goto l82
						l84:
							position, tokenIndex, depth = position82, tokenIndex82, depth82
							{
								position86, tokenIndex86, depth86 := position, tokenIndex, depth
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l87
								}
								position++
								goto l86
							l87:
								position, tokenIndex, depth = position86, tokenIndex86, depth86
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l85
								}
								position++
							}
						l86:
							goto l82
						l85:
							position, tokenIndex, depth = position82, tokenIndex82, depth82
							if buffer[position] != rune('-') {
								goto l88
							}
							position++
							goto l82
						l88:
							position, tokenIndex, depth = position82, tokenIndex82, depth82
							if buffer[position] != rune('_') {
								goto l89
							}
							position++
							goto l82
						l89:
							position, tokenIndex, depth = position82, tokenIndex82, depth82
							if buffer[position] != rune('.') {
								goto l81
							}
							position++
						}
					l82:
						goto l80
					l81:
						position, tokenIndex, depth = position81, tokenIndex81, depth81
					}
					depth--
					add(rulePegText, position79)
				}
				if !rules[ruleAction14]() {
					goto l77
				}
				depth--
				add(rulevalue, position78)
			}
			return true
		l77:
			position, tokenIndex, depth = position77, tokenIndex77, depth77
			return false
		},
		/* 17 leaderChar <- <([a-z] / [A-Z] / ([0-9] / [0-9]) / '.' / '_')> */
		func() bool {
			position90, tokenIndex90, depth90 := position, tokenIndex, depth
			{
				position91 := position
				depth++
				{
					position92, tokenIndex92, depth92 := position, tokenIndex, depth
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l93
					}
					position++
					goto l92
				l93:
					position, tokenIndex, depth = position92, tokenIndex92, depth92
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l94
					}
					position++
					goto l92
				l94:
					position, tokenIndex, depth = position92, tokenIndex92, depth92
					{
						position96, tokenIndex96, depth96 := position, tokenIndex, depth
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l97
						}
						position++
						goto l96
					l97:
						position, tokenIndex, depth = position96, tokenIndex96, depth96
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l95
						}
						position++
					}
				l96:
					goto l92
				l95:
					position, tokenIndex, depth = position92, tokenIndex92, depth92
					if buffer[position] != rune('.') {
						goto l98
					}
					position++
					goto l92
				l98:
					position, tokenIndex, depth = position92, tokenIndex92, depth92
					if buffer[position] != rune('_') {
						goto l90
					}
					position++
				}
			l92:
				depth--
				add(ruleleaderChar, position91)
			}
			return true
		l90:
			position, tokenIndex, depth = position90, tokenIndex90, depth90
			return false
		},
		/* 18 space <- <' '*> */
		func() bool {
			{
				position100 := position
				depth++
			l101:
				{
					position102, tokenIndex102, depth102 := position, tokenIndex, depth
					if buffer[position] != rune(' ') {
						goto l102
					}
					position++
					goto l101
				l102:
					position, tokenIndex, depth = position102, tokenIndex102, depth102
				}
				depth--
				add(rulespace, position100)
			}
			return true
		},
		/* 19 q <- <('q' '(' <(!')' .)*> ')' Action15)> */
		func() bool {
			position103, tokenIndex103, depth103 := position, tokenIndex, depth
			{
				position104 := position
				depth++
				if buffer[position] != rune('q') {
					goto l103
				}
				position++
				if buffer[position] != rune('(') {
					goto l103
				}
				position++
				{
					position105 := position
					depth++
				l106:
					{
						position107, tokenIndex107, depth107 := position, tokenIndex, depth
						{
							position108, tokenIndex108, depth108 := position, tokenIndex, depth
							if buffer[position] != rune(')') {
								goto l108
							}
							position++
							goto l107
						l108:
							position, tokenIndex, depth = position108, tokenIndex108, depth108
						}
						if !matchDot() {
							goto l107
						}
						goto l106
					l107:
						position, tokenIndex, depth = position107, tokenIndex107, depth107
					}
					depth--
					add(rulePegText, position105)
				}
				if buffer[position] != rune(')') {
					goto l103
				}
				position++
				if !rules[ruleAction15]() {
					goto l103
				}
				depth--
				add(ruleq, position104)
			}
			return true
		l103:
			position, tokenIndex, depth = position103, tokenIndex103, depth103
			return false
		},
		/* 21 Action0 <- <{ p.addBraceStart() }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 22 null <- <> */
		func() bool {
			{
				position112 := position
				depth++
				depth--
				add(rulenull, position112)
			}
			return true
		},
		/* 23 Action1 <- <{ p.addOperator(operatorIntersect) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 24 Action2 <- <{ p.addOperator(operatorSubtract) }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 25 Action3 <- <{ p.addOperator(operatorUnion) }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 26 Action4 <- <{ p.addBraces() }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 27 Action5 <- <{ p.addGroupQuery() }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 28 Action6 <- <{ p.addClusterLookup() }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 29 Action7 <- <{ p.addGroupLookup() }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 30 Action8 <- <{ p.addKeyLookup() }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 31 Action9 <- <{ p.addLocalClusterLookup(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 32 Action10 <- <{ p.addFunction(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 33 Action11 <- <{ p.addFuncArg() }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		/* 34 Action12 <- <{ p.addFuncArg() }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		nil,
		/* 36 Action13 <- <{ p.addRegex(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 37 Action14 <- <{ p.addValue(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		/* 38 Action15 <- <{ p.addConstant(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
	}
	p.rules = rules
}
