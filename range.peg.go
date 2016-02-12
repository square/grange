package grange

import (
	"fmt"
	"math"
	"sort"
	"strconv"
)

const end_symbol rune = 1114112

/* The rule types inferred from the grammar are below. */
type pegRule uint8

const (
	ruleUnknown pegRule = iota
	ruleexpression
	rulecombinedexpr
	rulerangeexpr
	rulecombinators
	ruleintersect
	ruleexclude
	ruleunion
	rulebraces
	rulebrackets
	ruleclusterq
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
	ruleconst
	ruleq
	rulequoted
	ruleAction0
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
	ruleAction13
	ruleAction14
	rulePegText
	ruleAction15
	ruleAction16
	ruleAction17
	ruleAction18

	rulePre_
	rule_In_
	rule_Suf
)

var rul3s = [...]string{
	"Unknown",
	"expression",
	"combinedexpr",
	"rangeexpr",
	"combinators",
	"intersect",
	"exclude",
	"union",
	"braces",
	"brackets",
	"clusterq",
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
	"const",
	"q",
	"quoted",
	"Action0",
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
	"Action14",
	"PegText",
	"Action15",
	"Action16",
	"Action17",
	"Action18",

	"Pre_",
	"_In_",
	"_Suf",
}

type tokenTree interface {
	Print()
	PrintSyntax()
	PrintSyntaxTree(buffer string)
	Add(rule pegRule, begin, end, next uint32, depth int)
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
		fmt.Printf("\x1B[34m%v\x1B[m %v\n", rul3s[node.pegRule], strconv.Quote(string(([]rune(buffer)[node.begin:node.end]))))
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
type token32 struct {
	pegRule
	begin, end, next uint32
}

func (t *token32) isZero() bool {
	return t.pegRule == ruleUnknown && t.begin == 0 && t.end == 0 && t.next == 0
}

func (t *token32) isParentOf(u token32) bool {
	return t.begin <= u.begin && t.end >= u.end && t.next > u.next
}

func (t *token32) getToken32() token32 {
	return token32{pegRule: t.pegRule, begin: uint32(t.begin), end: uint32(t.end), next: uint32(t.next)}
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
		token.next = uint32(i)
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
			state, S.pegRule, S.begin, S.end, S.next, S.leaf = (state+1)%8, t.pegRule, t.begin, t.end, uint32(depth), leaf
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
		fmt.Printf("\x1B[34m%v\x1B[m %v\n", rul3s[token.pegRule], strconv.Quote(string(([]rune(buffer)[token.begin:token.end]))))
	}
}

func (t *tokens32) Add(rule pegRule, begin, end, depth uint32, index int) {
	t.tree[index] = token32{pegRule: rule, begin: uint32(begin), end: uint32(end), next: uint32(depth)}
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

/*func (t *tokens16) Expand(index int) tokenTree {
	tree := t.tree
	if index >= len(tree) {
		expanded := make([]token32, 2 * len(tree))
		for i, v := range tree {
			expanded[i] = v.getToken32()
		}
		return &tokens32{tree: expanded}
	}
	return nil
}*/

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
	rules  [46]func() bool
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
	for i, c := range []rune(buffer) {
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
	buffer, _buffer, text, begin, end := p.Buffer, p.buffer, "", 0, 0
	for token := range p.tokenTree.Tokens() {
		switch token.pegRule {

		case rulePegText:
			begin, end = int(token.begin), int(token.end)
			text = string(_buffer[begin:end])

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
			p.addClusterQuery()
		case ruleAction6:
			p.addGroupQuery()
		case ruleAction7:
			p.addValue(buffer[begin:end])
			p.addClusterLookup()
		case ruleAction8:
			p.addClusterLookup()
		case ruleAction9:
			p.addGroupLookup()
		case ruleAction10:
			p.addKeyLookup()
		case ruleAction11:
			p.addLocalClusterLookup()
		case ruleAction12:
			p.addFunction(buffer[begin:end])
		case ruleAction13:
			p.addFuncArg()
		case ruleAction14:
			p.addFuncArg()
		case ruleAction15:
			p.addRegex(buffer[begin:end])
		case ruleAction16:
			p.addValue(buffer[begin:end])
		case ruleAction17:
			p.addConstant(buffer[begin:end])
		case ruleAction18:
			p.addConstant(buffer[begin:end])

		}
	}
	_, _, _, _, _ = buffer, _buffer, text, begin, end
}

func (p *rangeQuery) Init() {
	p.buffer = []rune(p.Buffer)
	if len(p.buffer) == 0 || p.buffer[len(p.buffer)-1] != end_symbol {
		p.buffer = append(p.buffer, end_symbol)
	}

	var tree tokenTree = &tokens32{tree: make([]token32, math.MaxInt16)}
	position, depth, tokenIndex, buffer, _rules := uint32(0), uint32(0), 0, p.buffer, p.rules

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

	add := func(rule pegRule, begin uint32) {
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

	_rules = [...]func() bool{
		nil,
		/* 0 expression <- <(combinedexpr? !.)> */
		func() bool {
			position0, tokenIndex0, depth0 := position, tokenIndex, depth
			{
				position1 := position
				depth++
				{
					position2, tokenIndex2, depth2 := position, tokenIndex, depth
					if !_rules[rulecombinedexpr]() {
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
		/* 1 combinedexpr <- <(rangeexpr combinators?)> */
		func() bool {
			position5, tokenIndex5, depth5 := position, tokenIndex, depth
			{
				position6 := position
				depth++
				if !_rules[rulerangeexpr]() {
					goto l5
				}
				{
					position7, tokenIndex7, depth7 := position, tokenIndex, depth
					if !_rules[rulecombinators]() {
						goto l7
					}
					goto l8
				l7:
					position, tokenIndex, depth = position7, tokenIndex7, depth7
				}
			l8:
				depth--
				add(rulecombinedexpr, position6)
			}
			return true
		l5:
			position, tokenIndex, depth = position5, tokenIndex5, depth5
			return false
		},
		/* 2 rangeexpr <- <(space (const / function / cluster / clusterq / group / groupq / localkey / regex / value / brackets / (Action0 braces)) space)> */
		func() bool {
			position9, tokenIndex9, depth9 := position, tokenIndex, depth
			{
				position10 := position
				depth++
				if !_rules[rulespace]() {
					goto l9
				}
				{
					position11, tokenIndex11, depth11 := position, tokenIndex, depth
					if !_rules[ruleconst]() {
						goto l12
					}
					goto l11
				l12:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[rulefunction]() {
						goto l13
					}
					goto l11
				l13:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[rulecluster]() {
						goto l14
					}
					goto l11
				l14:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[ruleclusterq]() {
						goto l15
					}
					goto l11
				l15:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[rulegroup]() {
						goto l16
					}
					goto l11
				l16:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[rulegroupq]() {
						goto l17
					}
					goto l11
				l17:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[rulelocalkey]() {
						goto l18
					}
					goto l11
				l18:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[ruleregex]() {
						goto l19
					}
					goto l11
				l19:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[rulevalue]() {
						goto l20
					}
					goto l11
				l20:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[rulebrackets]() {
						goto l21
					}
					goto l11
				l21:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					if !_rules[ruleAction0]() {
						goto l9
					}
					if !_rules[rulebraces]() {
						goto l9
					}
				}
			l11:
				if !_rules[rulespace]() {
					goto l9
				}
				depth--
				add(rulerangeexpr, position10)
			}
			return true
		l9:
			position, tokenIndex, depth = position9, tokenIndex9, depth9
			return false
		},
		/* 3 combinators <- <(space (union / intersect / exclude / braces))> */
		func() bool {
			position22, tokenIndex22, depth22 := position, tokenIndex, depth
			{
				position23 := position
				depth++
				if !_rules[rulespace]() {
					goto l22
				}
				{
					position24, tokenIndex24, depth24 := position, tokenIndex, depth
					if !_rules[ruleunion]() {
						goto l25
					}
					goto l24
				l25:
					position, tokenIndex, depth = position24, tokenIndex24, depth24
					if !_rules[ruleintersect]() {
						goto l26
					}
					goto l24
				l26:
					position, tokenIndex, depth = position24, tokenIndex24, depth24
					if !_rules[ruleexclude]() {
						goto l27
					}
					goto l24
				l27:
					position, tokenIndex, depth = position24, tokenIndex24, depth24
					if !_rules[rulebraces]() {
						goto l22
					}
				}
			l24:
				depth--
				add(rulecombinators, position23)
			}
			return true
		l22:
			position, tokenIndex, depth = position22, tokenIndex22, depth22
			return false
		},
		/* 4 intersect <- <('&' rangeexpr Action1 combinators?)> */
		func() bool {
			position28, tokenIndex28, depth28 := position, tokenIndex, depth
			{
				position29 := position
				depth++
				if buffer[position] != rune('&') {
					goto l28
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l28
				}
				if !_rules[ruleAction1]() {
					goto l28
				}
				{
					position30, tokenIndex30, depth30 := position, tokenIndex, depth
					if !_rules[rulecombinators]() {
						goto l30
					}
					goto l31
				l30:
					position, tokenIndex, depth = position30, tokenIndex30, depth30
				}
			l31:
				depth--
				add(ruleintersect, position29)
			}
			return true
		l28:
			position, tokenIndex, depth = position28, tokenIndex28, depth28
			return false
		},
		/* 5 exclude <- <('-' rangeexpr Action2 combinators?)> */
		func() bool {
			position32, tokenIndex32, depth32 := position, tokenIndex, depth
			{
				position33 := position
				depth++
				if buffer[position] != rune('-') {
					goto l32
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l32
				}
				if !_rules[ruleAction2]() {
					goto l32
				}
				{
					position34, tokenIndex34, depth34 := position, tokenIndex, depth
					if !_rules[rulecombinators]() {
						goto l34
					}
					goto l35
				l34:
					position, tokenIndex, depth = position34, tokenIndex34, depth34
				}
			l35:
				depth--
				add(ruleexclude, position33)
			}
			return true
		l32:
			position, tokenIndex, depth = position32, tokenIndex32, depth32
			return false
		},
		/* 6 union <- <(',' rangeexpr Action3 combinators?)> */
		func() bool {
			position36, tokenIndex36, depth36 := position, tokenIndex, depth
			{
				position37 := position
				depth++
				if buffer[position] != rune(',') {
					goto l36
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l36
				}
				if !_rules[ruleAction3]() {
					goto l36
				}
				{
					position38, tokenIndex38, depth38 := position, tokenIndex, depth
					if !_rules[rulecombinators]() {
						goto l38
					}
					goto l39
				l38:
					position, tokenIndex, depth = position38, tokenIndex38, depth38
				}
			l39:
				depth--
				add(ruleunion, position37)
			}
			return true
		l36:
			position, tokenIndex, depth = position36, tokenIndex36, depth36
			return false
		},
		/* 7 braces <- <('{' combinedexpr? '}' rangeexpr? Action4)> */
		func() bool {
			position40, tokenIndex40, depth40 := position, tokenIndex, depth
			{
				position41 := position
				depth++
				if buffer[position] != rune('{') {
					goto l40
				}
				position++
				{
					position42, tokenIndex42, depth42 := position, tokenIndex, depth
					if !_rules[rulecombinedexpr]() {
						goto l42
					}
					goto l43
				l42:
					position, tokenIndex, depth = position42, tokenIndex42, depth42
				}
			l43:
				if buffer[position] != rune('}') {
					goto l40
				}
				position++
				{
					position44, tokenIndex44, depth44 := position, tokenIndex, depth
					if !_rules[rulerangeexpr]() {
						goto l44
					}
					goto l45
				l44:
					position, tokenIndex, depth = position44, tokenIndex44, depth44
				}
			l45:
				if !_rules[ruleAction4]() {
					goto l40
				}
				depth--
				add(rulebraces, position41)
			}
			return true
		l40:
			position, tokenIndex, depth = position40, tokenIndex40, depth40
			return false
		},
		/* 8 brackets <- <('(' combinedexpr? ')')> */
		func() bool {
			position46, tokenIndex46, depth46 := position, tokenIndex, depth
			{
				position47 := position
				depth++
				if buffer[position] != rune('(') {
					goto l46
				}
				position++
				{
					position48, tokenIndex48, depth48 := position, tokenIndex, depth
					if !_rules[rulecombinedexpr]() {
						goto l48
					}
					goto l49
				l48:
					position, tokenIndex, depth = position48, tokenIndex48, depth48
				}
			l49:
				if buffer[position] != rune(')') {
					goto l46
				}
				position++
				depth--
				add(rulebrackets, position47)
			}
			return true
		l46:
			position, tokenIndex, depth = position46, tokenIndex46, depth46
			return false
		},
		/* 9 clusterq <- <('*' rangeexpr Action5)> */
		func() bool {
			position50, tokenIndex50, depth50 := position, tokenIndex, depth
			{
				position51 := position
				depth++
				if buffer[position] != rune('*') {
					goto l50
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l50
				}
				if !_rules[ruleAction5]() {
					goto l50
				}
				depth--
				add(ruleclusterq, position51)
			}
			return true
		l50:
			position, tokenIndex, depth = position50, tokenIndex50, depth50
			return false
		},
		/* 10 groupq <- <('?' rangeexpr Action6)> */
		func() bool {
			position52, tokenIndex52, depth52 := position, tokenIndex, depth
			{
				position53 := position
				depth++
				if buffer[position] != rune('?') {
					goto l52
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l52
				}
				if !_rules[ruleAction6]() {
					goto l52
				}
				depth--
				add(rulegroupq, position53)
			}
			return true
		l52:
			position, tokenIndex, depth = position52, tokenIndex52, depth52
			return false
		},
		/* 11 cluster <- <(('%' literal Action7 key?) / ('%' rangeexpr Action8 key?))> */
		func() bool {
			position54, tokenIndex54, depth54 := position, tokenIndex, depth
			{
				position55 := position
				depth++
				{
					position56, tokenIndex56, depth56 := position, tokenIndex, depth
					if buffer[position] != rune('%') {
						goto l57
					}
					position++
					if !_rules[ruleliteral]() {
						goto l57
					}
					if !_rules[ruleAction7]() {
						goto l57
					}
					{
						position58, tokenIndex58, depth58 := position, tokenIndex, depth
						if !_rules[rulekey]() {
							goto l58
						}
						goto l59
					l58:
						position, tokenIndex, depth = position58, tokenIndex58, depth58
					}
				l59:
					goto l56
				l57:
					position, tokenIndex, depth = position56, tokenIndex56, depth56
					if buffer[position] != rune('%') {
						goto l54
					}
					position++
					if !_rules[rulerangeexpr]() {
						goto l54
					}
					if !_rules[ruleAction8]() {
						goto l54
					}
					{
						position60, tokenIndex60, depth60 := position, tokenIndex, depth
						if !_rules[rulekey]() {
							goto l60
						}
						goto l61
					l60:
						position, tokenIndex, depth = position60, tokenIndex60, depth60
					}
				l61:
				}
			l56:
				depth--
				add(rulecluster, position55)
			}
			return true
		l54:
			position, tokenIndex, depth = position54, tokenIndex54, depth54
			return false
		},
		/* 12 group <- <('@' rangeexpr Action9)> */
		func() bool {
			position62, tokenIndex62, depth62 := position, tokenIndex, depth
			{
				position63 := position
				depth++
				if buffer[position] != rune('@') {
					goto l62
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l62
				}
				if !_rules[ruleAction9]() {
					goto l62
				}
				depth--
				add(rulegroup, position63)
			}
			return true
		l62:
			position, tokenIndex, depth = position62, tokenIndex62, depth62
			return false
		},
		/* 13 key <- <(':' rangeexpr Action10)> */
		func() bool {
			position64, tokenIndex64, depth64 := position, tokenIndex, depth
			{
				position65 := position
				depth++
				if buffer[position] != rune(':') {
					goto l64
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l64
				}
				if !_rules[ruleAction10]() {
					goto l64
				}
				depth--
				add(rulekey, position65)
			}
			return true
		l64:
			position, tokenIndex, depth = position64, tokenIndex64, depth64
			return false
		},
		/* 14 localkey <- <('$' rangeexpr Action11)> */
		func() bool {
			position66, tokenIndex66, depth66 := position, tokenIndex, depth
			{
				position67 := position
				depth++
				if buffer[position] != rune('$') {
					goto l66
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l66
				}
				if !_rules[ruleAction11]() {
					goto l66
				}
				depth--
				add(rulelocalkey, position67)
			}
			return true
		l66:
			position, tokenIndex, depth = position66, tokenIndex66, depth66
			return false
		},
		/* 15 function <- <(literal Action12 '(' funcargs ')')> */
		func() bool {
			position68, tokenIndex68, depth68 := position, tokenIndex, depth
			{
				position69 := position
				depth++
				if !_rules[ruleliteral]() {
					goto l68
				}
				if !_rules[ruleAction12]() {
					goto l68
				}
				if buffer[position] != rune('(') {
					goto l68
				}
				position++
				if !_rules[rulefuncargs]() {
					goto l68
				}
				if buffer[position] != rune(')') {
					goto l68
				}
				position++
				depth--
				add(rulefunction, position69)
			}
			return true
		l68:
			position, tokenIndex, depth = position68, tokenIndex68, depth68
			return false
		},
		/* 16 funcargs <- <((combinedexpr? Action13 ';' funcargs) / (combinedexpr? Action14))> */
		func() bool {
			position70, tokenIndex70, depth70 := position, tokenIndex, depth
			{
				position71 := position
				depth++
				{
					position72, tokenIndex72, depth72 := position, tokenIndex, depth
					{
						position74, tokenIndex74, depth74 := position, tokenIndex, depth
						if !_rules[rulecombinedexpr]() {
							goto l74
						}
						goto l75
					l74:
						position, tokenIndex, depth = position74, tokenIndex74, depth74
					}
				l75:
					if !_rules[ruleAction13]() {
						goto l73
					}
					if buffer[position] != rune(';') {
						goto l73
					}
					position++
					if !_rules[rulefuncargs]() {
						goto l73
					}
					goto l72
				l73:
					position, tokenIndex, depth = position72, tokenIndex72, depth72
					{
						position76, tokenIndex76, depth76 := position, tokenIndex, depth
						if !_rules[rulecombinedexpr]() {
							goto l76
						}
						goto l77
					l76:
						position, tokenIndex, depth = position76, tokenIndex76, depth76
					}
				l77:
					if !_rules[ruleAction14]() {
						goto l70
					}
				}
			l72:
				depth--
				add(rulefuncargs, position71)
			}
			return true
		l70:
			position, tokenIndex, depth = position70, tokenIndex70, depth70
			return false
		},
		/* 17 regex <- <('/' <(!'/' .)*> '/' Action15)> */
		func() bool {
			position78, tokenIndex78, depth78 := position, tokenIndex, depth
			{
				position79 := position
				depth++
				if buffer[position] != rune('/') {
					goto l78
				}
				position++
				{
					position80 := position
					depth++
				l81:
					{
						position82, tokenIndex82, depth82 := position, tokenIndex, depth
						{
							position83, tokenIndex83, depth83 := position, tokenIndex, depth
							if buffer[position] != rune('/') {
								goto l83
							}
							position++
							goto l82
						l83:
							position, tokenIndex, depth = position83, tokenIndex83, depth83
						}
						if !matchDot() {
							goto l82
						}
						goto l81
					l82:
						position, tokenIndex, depth = position82, tokenIndex82, depth82
					}
					depth--
					add(rulePegText, position80)
				}
				if buffer[position] != rune('/') {
					goto l78
				}
				position++
				if !_rules[ruleAction15]() {
					goto l78
				}
				depth--
				add(ruleregex, position79)
			}
			return true
		l78:
			position, tokenIndex, depth = position78, tokenIndex78, depth78
			return false
		},
		/* 18 literal <- <<(leaderChar ([a-z] / [A-Z] / ([0-9] / [0-9]) / '-' / '_')*)>> */
		func() bool {
			position84, tokenIndex84, depth84 := position, tokenIndex, depth
			{
				position85 := position
				depth++
				{
					position86 := position
					depth++
					if !_rules[ruleleaderChar]() {
						goto l84
					}
				l87:
					{
						position88, tokenIndex88, depth88 := position, tokenIndex, depth
						{
							position89, tokenIndex89, depth89 := position, tokenIndex, depth
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l90
							}
							position++
							goto l89
						l90:
							position, tokenIndex, depth = position89, tokenIndex89, depth89
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l91
							}
							position++
							goto l89
						l91:
							position, tokenIndex, depth = position89, tokenIndex89, depth89
							{
								position93, tokenIndex93, depth93 := position, tokenIndex, depth
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l94
								}
								position++
								goto l93
							l94:
								position, tokenIndex, depth = position93, tokenIndex93, depth93
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l92
								}
								position++
							}
						l93:
							goto l89
						l92:
							position, tokenIndex, depth = position89, tokenIndex89, depth89
							if buffer[position] != rune('-') {
								goto l95
							}
							position++
							goto l89
						l95:
							position, tokenIndex, depth = position89, tokenIndex89, depth89
							if buffer[position] != rune('_') {
								goto l88
							}
							position++
						}
					l89:
						goto l87
					l88:
						position, tokenIndex, depth = position88, tokenIndex88, depth88
					}
					depth--
					add(rulePegText, position86)
				}
				depth--
				add(ruleliteral, position85)
			}
			return true
		l84:
			position, tokenIndex, depth = position84, tokenIndex84, depth84
			return false
		},
		/* 19 value <- <(<(leaderChar (':' / ([a-z] / [A-Z]) / ([0-9] / [0-9]) / '-' / '_' / '.')*)> Action16)> */
		func() bool {
			position96, tokenIndex96, depth96 := position, tokenIndex, depth
			{
				position97 := position
				depth++
				{
					position98 := position
					depth++
					if !_rules[ruleleaderChar]() {
						goto l96
					}
				l99:
					{
						position100, tokenIndex100, depth100 := position, tokenIndex, depth
						{
							position101, tokenIndex101, depth101 := position, tokenIndex, depth
							if buffer[position] != rune(':') {
								goto l102
							}
							position++
							goto l101
						l102:
							position, tokenIndex, depth = position101, tokenIndex101, depth101
							{
								position104, tokenIndex104, depth104 := position, tokenIndex, depth
								if c := buffer[position]; c < rune('a') || c > rune('z') {
									goto l105
								}
								position++
								goto l104
							l105:
								position, tokenIndex, depth = position104, tokenIndex104, depth104
								if c := buffer[position]; c < rune('A') || c > rune('Z') {
									goto l103
								}
								position++
							}
						l104:
							goto l101
						l103:
							position, tokenIndex, depth = position101, tokenIndex101, depth101
							{
								position107, tokenIndex107, depth107 := position, tokenIndex, depth
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l108
								}
								position++
								goto l107
							l108:
								position, tokenIndex, depth = position107, tokenIndex107, depth107
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l106
								}
								position++
							}
						l107:
							goto l101
						l106:
							position, tokenIndex, depth = position101, tokenIndex101, depth101
							if buffer[position] != rune('-') {
								goto l109
							}
							position++
							goto l101
						l109:
							position, tokenIndex, depth = position101, tokenIndex101, depth101
							if buffer[position] != rune('_') {
								goto l110
							}
							position++
							goto l101
						l110:
							position, tokenIndex, depth = position101, tokenIndex101, depth101
							if buffer[position] != rune('.') {
								goto l100
							}
							position++
						}
					l101:
						goto l99
					l100:
						position, tokenIndex, depth = position100, tokenIndex100, depth100
					}
					depth--
					add(rulePegText, position98)
				}
				if !_rules[ruleAction16]() {
					goto l96
				}
				depth--
				add(rulevalue, position97)
			}
			return true
		l96:
			position, tokenIndex, depth = position96, tokenIndex96, depth96
			return false
		},
		/* 20 leaderChar <- <([a-z] / [A-Z] / ([0-9] / [0-9]) / '.' / '_')> */
		func() bool {
			position111, tokenIndex111, depth111 := position, tokenIndex, depth
			{
				position112 := position
				depth++
				{
					position113, tokenIndex113, depth113 := position, tokenIndex, depth
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l114
					}
					position++
					goto l113
				l114:
					position, tokenIndex, depth = position113, tokenIndex113, depth113
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l115
					}
					position++
					goto l113
				l115:
					position, tokenIndex, depth = position113, tokenIndex113, depth113
					{
						position117, tokenIndex117, depth117 := position, tokenIndex, depth
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l118
						}
						position++
						goto l117
					l118:
						position, tokenIndex, depth = position117, tokenIndex117, depth117
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l116
						}
						position++
					}
				l117:
					goto l113
				l116:
					position, tokenIndex, depth = position113, tokenIndex113, depth113
					if buffer[position] != rune('.') {
						goto l119
					}
					position++
					goto l113
				l119:
					position, tokenIndex, depth = position113, tokenIndex113, depth113
					if buffer[position] != rune('_') {
						goto l111
					}
					position++
				}
			l113:
				depth--
				add(ruleleaderChar, position112)
			}
			return true
		l111:
			position, tokenIndex, depth = position111, tokenIndex111, depth111
			return false
		},
		/* 21 space <- <' '*> */
		func() bool {
			{
				position121 := position
				depth++
			l122:
				{
					position123, tokenIndex123, depth123 := position, tokenIndex, depth
					if buffer[position] != rune(' ') {
						goto l123
					}
					position++
					goto l122
				l123:
					position, tokenIndex, depth = position123, tokenIndex123, depth123
				}
				depth--
				add(rulespace, position121)
			}
			return true
		},
		/* 22 const <- <(q / quoted)> */
		func() bool {
			position124, tokenIndex124, depth124 := position, tokenIndex, depth
			{
				position125 := position
				depth++
				{
					position126, tokenIndex126, depth126 := position, tokenIndex, depth
					if !_rules[ruleq]() {
						goto l127
					}
					goto l126
				l127:
					position, tokenIndex, depth = position126, tokenIndex126, depth126
					if !_rules[rulequoted]() {
						goto l124
					}
				}
			l126:
				depth--
				add(ruleconst, position125)
			}
			return true
		l124:
			position, tokenIndex, depth = position124, tokenIndex124, depth124
			return false
		},
		/* 23 q <- <('q' '(' <(!')' .)*> ')' Action17)> */
		func() bool {
			position128, tokenIndex128, depth128 := position, tokenIndex, depth
			{
				position129 := position
				depth++
				if buffer[position] != rune('q') {
					goto l128
				}
				position++
				if buffer[position] != rune('(') {
					goto l128
				}
				position++
				{
					position130 := position
					depth++
				l131:
					{
						position132, tokenIndex132, depth132 := position, tokenIndex, depth
						{
							position133, tokenIndex133, depth133 := position, tokenIndex, depth
							if buffer[position] != rune(')') {
								goto l133
							}
							position++
							goto l132
						l133:
							position, tokenIndex, depth = position133, tokenIndex133, depth133
						}
						if !matchDot() {
							goto l132
						}
						goto l131
					l132:
						position, tokenIndex, depth = position132, tokenIndex132, depth132
					}
					depth--
					add(rulePegText, position130)
				}
				if buffer[position] != rune(')') {
					goto l128
				}
				position++
				if !_rules[ruleAction17]() {
					goto l128
				}
				depth--
				add(ruleq, position129)
			}
			return true
		l128:
			position, tokenIndex, depth = position128, tokenIndex128, depth128
			return false
		},
		/* 24 quoted <- <('"' <(!'"' .)*> '"' Action18)> */
		func() bool {
			position134, tokenIndex134, depth134 := position, tokenIndex, depth
			{
				position135 := position
				depth++
				if buffer[position] != rune('"') {
					goto l134
				}
				position++
				{
					position136 := position
					depth++
				l137:
					{
						position138, tokenIndex138, depth138 := position, tokenIndex, depth
						{
							position139, tokenIndex139, depth139 := position, tokenIndex, depth
							if buffer[position] != rune('"') {
								goto l139
							}
							position++
							goto l138
						l139:
							position, tokenIndex, depth = position139, tokenIndex139, depth139
						}
						if !matchDot() {
							goto l138
						}
						goto l137
					l138:
						position, tokenIndex, depth = position138, tokenIndex138, depth138
					}
					depth--
					add(rulePegText, position136)
				}
				if buffer[position] != rune('"') {
					goto l134
				}
				position++
				if !_rules[ruleAction18]() {
					goto l134
				}
				depth--
				add(rulequoted, position135)
			}
			return true
		l134:
			position, tokenIndex, depth = position134, tokenIndex134, depth134
			return false
		},
		/* 26 Action0 <- <{ p.addBraceStart() }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 27 Action1 <- <{ p.addOperator(operatorIntersect) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 28 Action2 <- <{ p.addOperator(operatorSubtract) }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 29 Action3 <- <{ p.addOperator(operatorUnion) }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 30 Action4 <- <{ p.addBraces() }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 31 Action5 <- <{ p.addClusterQuery() }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 32 Action6 <- <{ p.addGroupQuery() }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 33 Action7 <- <{ p.addValue(buffer[begin:end]); p.addClusterLookup() }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 34 Action8 <- <{ p.addClusterLookup() }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 35 Action9 <- <{ p.addGroupLookup() }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 36 Action10 <- <{ p.addKeyLookup() }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 37 Action11 <- <{ p.addLocalClusterLookup() }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		/* 38 Action12 <- <{ p.addFunction(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		/* 39 Action13 <- <{ p.addFuncArg() }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 40 Action14 <- <{ p.addFuncArg() }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		nil,
		/* 42 Action15 <- <{ p.addRegex(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
		/* 43 Action16 <- <{ p.addValue(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction16, position)
			}
			return true
		},
		/* 44 Action17 <- <{ p.addConstant(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction17, position)
			}
			return true
		},
		/* 45 Action18 <- <{ p.addConstant(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction18, position)
			}
			return true
		},
	}
	p.rules = _rules
}
