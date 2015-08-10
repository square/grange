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
			p.addLocalClusterLookup(buffer[begin:end])
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
	_, _, _, _ = buffer, text, begin, end
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
		/* 2 rangeexpr <- <(space (const / function / ((&('{') (Action0 braces)) | (&('(') brackets) | (&('/') regex) | (&('$') localkey) | (&('?') groupq) | (&('@') group) | (&('*') clusterq) | (&('%') cluster) | (&('.' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '_' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z') value))) space)> */
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
					{
						position13 := position
						depth++
						{
							position14, tokenIndex14, depth14 := position, tokenIndex, depth
							{
								position16 := position
								depth++
								if buffer[position] != rune('q') {
									goto l15
								}
								position++
								if buffer[position] != rune('(') {
									goto l15
								}
								position++
								{
									position17 := position
									depth++
								l18:
									{
										position19, tokenIndex19, depth19 := position, tokenIndex, depth
										{
											position20, tokenIndex20, depth20 := position, tokenIndex, depth
											if buffer[position] != rune(')') {
												goto l20
											}
											position++
											goto l19
										l20:
											position, tokenIndex, depth = position20, tokenIndex20, depth20
										}
										if !matchDot() {
											goto l19
										}
										goto l18
									l19:
										position, tokenIndex, depth = position19, tokenIndex19, depth19
									}
									depth--
									add(rulePegText, position17)
								}
								if buffer[position] != rune(')') {
									goto l15
								}
								position++
								{
									add(ruleAction17, position)
								}
								depth--
								add(ruleq, position16)
							}
							goto l14
						l15:
							position, tokenIndex, depth = position14, tokenIndex14, depth14
							{
								position22 := position
								depth++
								if buffer[position] != rune('"') {
									goto l12
								}
								position++
								{
									position23 := position
									depth++
								l24:
									{
										position25, tokenIndex25, depth25 := position, tokenIndex, depth
										{
											position26, tokenIndex26, depth26 := position, tokenIndex, depth
											if buffer[position] != rune('"') {
												goto l26
											}
											position++
											goto l25
										l26:
											position, tokenIndex, depth = position26, tokenIndex26, depth26
										}
										if !matchDot() {
											goto l25
										}
										goto l24
									l25:
										position, tokenIndex, depth = position25, tokenIndex25, depth25
									}
									depth--
									add(rulePegText, position23)
								}
								if buffer[position] != rune('"') {
									goto l12
								}
								position++
								{
									add(ruleAction18, position)
								}
								depth--
								add(rulequoted, position22)
							}
						}
					l14:
						depth--
						add(ruleconst, position13)
					}
					goto l11
				l12:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					{
						position29 := position
						depth++
						if !_rules[ruleliteral]() {
							goto l28
						}
						{
							add(ruleAction12, position)
						}
						if buffer[position] != rune('(') {
							goto l28
						}
						position++
						if !_rules[rulefuncargs]() {
							goto l28
						}
						if buffer[position] != rune(')') {
							goto l28
						}
						position++
						depth--
						add(rulefunction, position29)
					}
					goto l11
				l28:
					position, tokenIndex, depth = position11, tokenIndex11, depth11
					{
						switch buffer[position] {
						case '{':
							{
								add(ruleAction0, position)
							}
							if !_rules[rulebraces]() {
								goto l9
							}
							break
						case '(':
							{
								position33 := position
								depth++
								if buffer[position] != rune('(') {
									goto l9
								}
								position++
								{
									position34, tokenIndex34, depth34 := position, tokenIndex, depth
									if !_rules[rulecombinedexpr]() {
										goto l34
									}
									goto l35
								l34:
									position, tokenIndex, depth = position34, tokenIndex34, depth34
								}
							l35:
								if buffer[position] != rune(')') {
									goto l9
								}
								position++
								depth--
								add(rulebrackets, position33)
							}
							break
						case '/':
							{
								position36 := position
								depth++
								if buffer[position] != rune('/') {
									goto l9
								}
								position++
								{
									position37 := position
									depth++
								l38:
									{
										position39, tokenIndex39, depth39 := position, tokenIndex, depth
										{
											position40, tokenIndex40, depth40 := position, tokenIndex, depth
											if buffer[position] != rune('/') {
												goto l40
											}
											position++
											goto l39
										l40:
											position, tokenIndex, depth = position40, tokenIndex40, depth40
										}
										if !matchDot() {
											goto l39
										}
										goto l38
									l39:
										position, tokenIndex, depth = position39, tokenIndex39, depth39
									}
									depth--
									add(rulePegText, position37)
								}
								if buffer[position] != rune('/') {
									goto l9
								}
								position++
								{
									add(ruleAction15, position)
								}
								depth--
								add(ruleregex, position36)
							}
							break
						case '$':
							{
								position42 := position
								depth++
								if buffer[position] != rune('$') {
									goto l9
								}
								position++
								if !_rules[ruleliteral]() {
									goto l9
								}
								{
									add(ruleAction11, position)
								}
								depth--
								add(rulelocalkey, position42)
							}
							break
						case '?':
							{
								position44 := position
								depth++
								if buffer[position] != rune('?') {
									goto l9
								}
								position++
								if !_rules[rulerangeexpr]() {
									goto l9
								}
								{
									add(ruleAction6, position)
								}
								depth--
								add(rulegroupq, position44)
							}
							break
						case '@':
							{
								position46 := position
								depth++
								if buffer[position] != rune('@') {
									goto l9
								}
								position++
								if !_rules[rulerangeexpr]() {
									goto l9
								}
								{
									add(ruleAction9, position)
								}
								depth--
								add(rulegroup, position46)
							}
							break
						case '*':
							{
								position48 := position
								depth++
								if buffer[position] != rune('*') {
									goto l9
								}
								position++
								if !_rules[rulerangeexpr]() {
									goto l9
								}
								{
									add(ruleAction5, position)
								}
								depth--
								add(ruleclusterq, position48)
							}
							break
						case '%':
							{
								position50 := position
								depth++
								{
									position51, tokenIndex51, depth51 := position, tokenIndex, depth
									if buffer[position] != rune('%') {
										goto l52
									}
									position++
									if !_rules[ruleliteral]() {
										goto l52
									}
									{
										add(ruleAction7, position)
									}
									{
										position54, tokenIndex54, depth54 := position, tokenIndex, depth
										if !_rules[rulekey]() {
											goto l54
										}
										goto l55
									l54:
										position, tokenIndex, depth = position54, tokenIndex54, depth54
									}
								l55:
									goto l51
								l52:
									position, tokenIndex, depth = position51, tokenIndex51, depth51
									if buffer[position] != rune('%') {
										goto l9
									}
									position++
									if !_rules[rulerangeexpr]() {
										goto l9
									}
									{
										add(ruleAction8, position)
									}
									{
										position57, tokenIndex57, depth57 := position, tokenIndex, depth
										if !_rules[rulekey]() {
											goto l57
										}
										goto l58
									l57:
										position, tokenIndex, depth = position57, tokenIndex57, depth57
									}
								l58:
								}
							l51:
								depth--
								add(rulecluster, position50)
							}
							break
						default:
							{
								position59 := position
								depth++
								{
									position60 := position
									depth++
									if !_rules[ruleleaderChar]() {
										goto l9
									}
								l61:
									{
										position62, tokenIndex62, depth62 := position, tokenIndex, depth
										{
											switch buffer[position] {
											case '.':
												if buffer[position] != rune('.') {
													goto l62
												}
												position++
												break
											case '_':
												if buffer[position] != rune('_') {
													goto l62
												}
												position++
												break
											case '-':
												if buffer[position] != rune('-') {
													goto l62
												}
												position++
												break
											case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
												{
													position64, tokenIndex64, depth64 := position, tokenIndex, depth
													if c := buffer[position]; c < rune('0') || c > rune('9') {
														goto l65
													}
													position++
													goto l64
												l65:
													position, tokenIndex, depth = position64, tokenIndex64, depth64
													if c := buffer[position]; c < rune('0') || c > rune('9') {
														goto l62
													}
													position++
												}
											l64:
												break
											case ':':
												if buffer[position] != rune(':') {
													goto l62
												}
												position++
												break
											default:
												{
													position66, tokenIndex66, depth66 := position, tokenIndex, depth
													if c := buffer[position]; c < rune('a') || c > rune('z') {
														goto l67
													}
													position++
													goto l66
												l67:
													position, tokenIndex, depth = position66, tokenIndex66, depth66
													if c := buffer[position]; c < rune('A') || c > rune('Z') {
														goto l62
													}
													position++
												}
											l66:
												break
											}
										}

										goto l61
									l62:
										position, tokenIndex, depth = position62, tokenIndex62, depth62
									}
									depth--
									add(rulePegText, position60)
								}
								{
									add(ruleAction16, position)
								}
								depth--
								add(rulevalue, position59)
							}
							break
						}
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
		/* 3 combinators <- <(space ((&('{') braces) | (&('-') exclude) | (&('&') intersect) | (&(',') union)))> */
		func() bool {
			position69, tokenIndex69, depth69 := position, tokenIndex, depth
			{
				position70 := position
				depth++
				if !_rules[rulespace]() {
					goto l69
				}
				{
					switch buffer[position] {
					case '{':
						if !_rules[rulebraces]() {
							goto l69
						}
						break
					case '-':
						{
							position72 := position
							depth++
							if buffer[position] != rune('-') {
								goto l69
							}
							position++
							if !_rules[rulerangeexpr]() {
								goto l69
							}
							{
								add(ruleAction2, position)
							}
							{
								position74, tokenIndex74, depth74 := position, tokenIndex, depth
								if !_rules[rulecombinators]() {
									goto l74
								}
								goto l75
							l74:
								position, tokenIndex, depth = position74, tokenIndex74, depth74
							}
						l75:
							depth--
							add(ruleexclude, position72)
						}
						break
					case '&':
						{
							position76 := position
							depth++
							if buffer[position] != rune('&') {
								goto l69
							}
							position++
							if !_rules[rulerangeexpr]() {
								goto l69
							}
							{
								add(ruleAction1, position)
							}
							{
								position78, tokenIndex78, depth78 := position, tokenIndex, depth
								if !_rules[rulecombinators]() {
									goto l78
								}
								goto l79
							l78:
								position, tokenIndex, depth = position78, tokenIndex78, depth78
							}
						l79:
							depth--
							add(ruleintersect, position76)
						}
						break
					default:
						{
							position80 := position
							depth++
							if buffer[position] != rune(',') {
								goto l69
							}
							position++
							if !_rules[rulerangeexpr]() {
								goto l69
							}
							{
								add(ruleAction3, position)
							}
							{
								position82, tokenIndex82, depth82 := position, tokenIndex, depth
								if !_rules[rulecombinators]() {
									goto l82
								}
								goto l83
							l82:
								position, tokenIndex, depth = position82, tokenIndex82, depth82
							}
						l83:
							depth--
							add(ruleunion, position80)
						}
						break
					}
				}

				depth--
				add(rulecombinators, position70)
			}
			return true
		l69:
			position, tokenIndex, depth = position69, tokenIndex69, depth69
			return false
		},
		/* 4 intersect <- <('&' rangeexpr Action1 combinators?)> */
		nil,
		/* 5 exclude <- <('-' rangeexpr Action2 combinators?)> */
		nil,
		/* 6 union <- <(',' rangeexpr Action3 combinators?)> */
		nil,
		/* 7 braces <- <('{' combinedexpr? '}' rangeexpr? Action4)> */
		func() bool {
			position87, tokenIndex87, depth87 := position, tokenIndex, depth
			{
				position88 := position
				depth++
				if buffer[position] != rune('{') {
					goto l87
				}
				position++
				{
					position89, tokenIndex89, depth89 := position, tokenIndex, depth
					if !_rules[rulecombinedexpr]() {
						goto l89
					}
					goto l90
				l89:
					position, tokenIndex, depth = position89, tokenIndex89, depth89
				}
			l90:
				if buffer[position] != rune('}') {
					goto l87
				}
				position++
				{
					position91, tokenIndex91, depth91 := position, tokenIndex, depth
					if !_rules[rulerangeexpr]() {
						goto l91
					}
					goto l92
				l91:
					position, tokenIndex, depth = position91, tokenIndex91, depth91
				}
			l92:
				{
					add(ruleAction4, position)
				}
				depth--
				add(rulebraces, position88)
			}
			return true
		l87:
			position, tokenIndex, depth = position87, tokenIndex87, depth87
			return false
		},
		/* 8 brackets <- <('(' combinedexpr? ')')> */
		nil,
		/* 9 clusterq <- <('*' rangeexpr Action5)> */
		nil,
		/* 10 groupq <- <('?' rangeexpr Action6)> */
		nil,
		/* 11 cluster <- <(('%' literal Action7 key?) / ('%' rangeexpr Action8 key?))> */
		nil,
		/* 12 group <- <('@' rangeexpr Action9)> */
		nil,
		/* 13 key <- <(':' rangeexpr Action10)> */
		func() bool {
			position99, tokenIndex99, depth99 := position, tokenIndex, depth
			{
				position100 := position
				depth++
				if buffer[position] != rune(':') {
					goto l99
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l99
				}
				{
					add(ruleAction10, position)
				}
				depth--
				add(rulekey, position100)
			}
			return true
		l99:
			position, tokenIndex, depth = position99, tokenIndex99, depth99
			return false
		},
		/* 14 localkey <- <('$' literal Action11)> */
		nil,
		/* 15 function <- <(literal Action12 '(' funcargs ')')> */
		nil,
		/* 16 funcargs <- <((combinedexpr? Action13 ';' funcargs) / (combinedexpr? Action14))> */
		func() bool {
			{
				position105 := position
				depth++
				{
					position106, tokenIndex106, depth106 := position, tokenIndex, depth
					{
						position108, tokenIndex108, depth108 := position, tokenIndex, depth
						if !_rules[rulecombinedexpr]() {
							goto l108
						}
						goto l109
					l108:
						position, tokenIndex, depth = position108, tokenIndex108, depth108
					}
				l109:
					{
						add(ruleAction13, position)
					}
					if buffer[position] != rune(';') {
						goto l107
					}
					position++
					if !_rules[rulefuncargs]() {
						goto l107
					}
					goto l106
				l107:
					position, tokenIndex, depth = position106, tokenIndex106, depth106
					{
						position111, tokenIndex111, depth111 := position, tokenIndex, depth
						if !_rules[rulecombinedexpr]() {
							goto l111
						}
						goto l112
					l111:
						position, tokenIndex, depth = position111, tokenIndex111, depth111
					}
				l112:
					{
						add(ruleAction14, position)
					}
				}
			l106:
				depth--
				add(rulefuncargs, position105)
			}
			return true
		},
		/* 17 regex <- <('/' <(!'/' .)*> '/' Action15)> */
		nil,
		/* 18 literal <- <<(leaderChar ((&('_') '_') | (&('-') '-') | (&('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') ([0-9] / [0-9])) | (&('A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z') [A-Z]) | (&('a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z') [a-z]))*)>> */
		func() bool {
			position115, tokenIndex115, depth115 := position, tokenIndex, depth
			{
				position116 := position
				depth++
				{
					position117 := position
					depth++
					if !_rules[ruleleaderChar]() {
						goto l115
					}
				l118:
					{
						position119, tokenIndex119, depth119 := position, tokenIndex, depth
						{
							switch buffer[position] {
							case '_':
								if buffer[position] != rune('_') {
									goto l119
								}
								position++
								break
							case '-':
								if buffer[position] != rune('-') {
									goto l119
								}
								position++
								break
							case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
								{
									position121, tokenIndex121, depth121 := position, tokenIndex, depth
									if c := buffer[position]; c < rune('0') || c > rune('9') {
										goto l122
									}
									position++
									goto l121
								l122:
									position, tokenIndex, depth = position121, tokenIndex121, depth121
									if c := buffer[position]; c < rune('0') || c > rune('9') {
										goto l119
									}
									position++
								}
							l121:
								break
							case 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z':
								if c := buffer[position]; c < rune('A') || c > rune('Z') {
									goto l119
								}
								position++
								break
							default:
								if c := buffer[position]; c < rune('a') || c > rune('z') {
									goto l119
								}
								position++
								break
							}
						}

						goto l118
					l119:
						position, tokenIndex, depth = position119, tokenIndex119, depth119
					}
					depth--
					add(rulePegText, position117)
				}
				depth--
				add(ruleliteral, position116)
			}
			return true
		l115:
			position, tokenIndex, depth = position115, tokenIndex115, depth115
			return false
		},
		/* 19 value <- <(<(leaderChar ((&('.') '.') | (&('_') '_') | (&('-') '-') | (&('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') ([0-9] / [0-9])) | (&(':') ':') | (&('A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z') ([a-z] / [A-Z])))*)> Action16)> */
		nil,
		/* 20 leaderChar <- <((&('_') '_') | (&('.') '.') | (&('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') ([0-9] / [0-9])) | (&('A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z') [A-Z]) | (&('a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z') [a-z]))> */
		func() bool {
			position124, tokenIndex124, depth124 := position, tokenIndex, depth
			{
				position125 := position
				depth++
				{
					switch buffer[position] {
					case '_':
						if buffer[position] != rune('_') {
							goto l124
						}
						position++
						break
					case '.':
						if buffer[position] != rune('.') {
							goto l124
						}
						position++
						break
					case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
						{
							position127, tokenIndex127, depth127 := position, tokenIndex, depth
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l128
							}
							position++
							goto l127
						l128:
							position, tokenIndex, depth = position127, tokenIndex127, depth127
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l124
							}
							position++
						}
					l127:
						break
					case 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z':
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l124
						}
						position++
						break
					default:
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l124
						}
						position++
						break
					}
				}

				depth--
				add(ruleleaderChar, position125)
			}
			return true
		l124:
			position, tokenIndex, depth = position124, tokenIndex124, depth124
			return false
		},
		/* 21 space <- <' '*> */
		func() bool {
			{
				position130 := position
				depth++
			l131:
				{
					position132, tokenIndex132, depth132 := position, tokenIndex, depth
					if buffer[position] != rune(' ') {
						goto l132
					}
					position++
					goto l131
				l132:
					position, tokenIndex, depth = position132, tokenIndex132, depth132
				}
				depth--
				add(rulespace, position130)
			}
			return true
		},
		/* 22 const <- <(q / quoted)> */
		nil,
		/* 23 q <- <('q' '(' <(!')' .)*> ')' Action17)> */
		nil,
		/* 24 quoted <- <('"' <(!'"' .)*> '"' Action18)> */
		nil,
		/* 26 Action0 <- <{ p.addBraceStart() }> */
		nil,
		/* 27 Action1 <- <{ p.addOperator(operatorIntersect) }> */
		nil,
		/* 28 Action2 <- <{ p.addOperator(operatorSubtract) }> */
		nil,
		/* 29 Action3 <- <{ p.addOperator(operatorUnion) }> */
		nil,
		/* 30 Action4 <- <{ p.addBraces() }> */
		nil,
		/* 31 Action5 <- <{ p.addClusterQuery() }> */
		nil,
		/* 32 Action6 <- <{ p.addGroupQuery() }> */
		nil,
		/* 33 Action7 <- <{ p.addValue(buffer[begin:end]); p.addClusterLookup() }> */
		nil,
		/* 34 Action8 <- <{ p.addClusterLookup() }> */
		nil,
		/* 35 Action9 <- <{ p.addGroupLookup() }> */
		nil,
		/* 36 Action10 <- <{ p.addKeyLookup() }> */
		nil,
		/* 37 Action11 <- <{ p.addLocalClusterLookup(buffer[begin:end]) }> */
		nil,
		/* 38 Action12 <- <{ p.addFunction(buffer[begin:end]) }> */
		nil,
		/* 39 Action13 <- <{ p.addFuncArg() }> */
		nil,
		/* 40 Action14 <- <{ p.addFuncArg() }> */
		nil,
		nil,
		/* 42 Action15 <- <{ p.addRegex(buffer[begin:end]) }> */
		nil,
		/* 43 Action16 <- <{ p.addValue(buffer[begin:end]) }> */
		nil,
		/* 44 Action17 <- <{ p.addConstant(buffer[begin:end]) }> */
		nil,
		/* 45 Action18 <- <{ p.addConstant(buffer[begin:end]) }> */
		nil,
	}
	p.rules = _rules
}
