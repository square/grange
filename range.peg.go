package grange

//go:generate /home/xavier/Code/go/bin/peg range.peg

import (
	"fmt"
	"math"
	"sort"
	"strconv"
)

const endSymbol rune = 1114112

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
}

type token32 struct {
	pegRule
	begin, end uint32
}

func (t *token32) String() string {
	return fmt.Sprintf("\x1B[34m%v\x1B[m %v %v", rul3s[t.pegRule], t.begin, t.end)
}

type node32 struct {
	token32
	up, next *node32
}

func (node *node32) print(pretty bool, buffer string) {
	var print func(node *node32, depth int)
	print = func(node *node32, depth int) {
		for node != nil {
			for c := 0; c < depth; c++ {
				fmt.Printf(" ")
			}
			rule := rul3s[node.pegRule]
			quote := strconv.Quote(string(([]rune(buffer)[node.begin:node.end])))
			if !pretty {
				fmt.Printf("%v %v\n", rule, quote)
			} else {
				fmt.Printf("\x1B[34m%v\x1B[m %v\n", rule, quote)
			}
			if node.up != nil {
				print(node.up, depth+1)
			}
			node = node.next
		}
	}
	print(node, 0)
}

func (node *node32) Print(buffer string) {
	node.print(false, buffer)
}

func (node *node32) PrettyPrint(buffer string) {
	node.print(true, buffer)
}

type tokens32 struct {
	tree []token32
}

func (t *tokens32) Trim(length uint32) {
	t.tree = t.tree[:length]
}

func (t *tokens32) Print() {
	for _, token := range t.tree {
		fmt.Println(token.String())
	}
}

func (t *tokens32) AST() *node32 {
	type element struct {
		node *node32
		down *element
	}
	tokens := t.Tokens()
	var stack *element
	for _, token := range tokens {
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
	if stack != nil {
		return stack.node
	}
	return nil
}

func (t *tokens32) PrintSyntaxTree(buffer string) {
	t.AST().Print(buffer)
}

func (t *tokens32) PrettyPrintSyntaxTree(buffer string) {
	t.AST().PrettyPrint(buffer)
}

func (t *tokens32) Add(rule pegRule, begin, end, index uint32) {
	if tree := t.tree; int(index) >= len(tree) {
		expanded := make([]token32, 2*len(tree))
		copy(expanded, tree)
		t.tree = expanded
	}
	t.tree[index] = token32{
		pegRule: rule,
		begin:   begin,
		end:     end,
	}
}

func (t *tokens32) Tokens() []token32 {
	return t.tree
}

type rangeQuery struct {
	currentLiteral string
	nodeStack      []parserNode

	Buffer string
	buffer []rune
	rules  [46]func() bool
	parse  func(rule ...int) error
	reset  func()
	Pretty bool
	tokens32
}

func (p *rangeQuery) Parse(rule ...int) error {
	return p.parse(rule...)
}

func (p *rangeQuery) Reset() {
	p.reset()
}

type textPosition struct {
	line, symbol int
}

type textPositionMap map[int]textPosition

func translatePositions(buffer []rune, positions []int) textPositionMap {
	length, translations, j, line, symbol := len(positions), make(textPositionMap, len(positions)), 0, 1, 0
	sort.Ints(positions)

search:
	for i, c := range buffer {
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
	p   *rangeQuery
	max token32
}

func (e *parseError) Error() string {
	tokens, error := []token32{e.max}, "\n"
	positions, p := make([]int, 2*len(tokens)), 0
	for _, token := range tokens {
		positions[p], p = int(token.begin), p+1
		positions[p], p = int(token.end), p+1
	}
	translations := translatePositions(e.p.buffer, positions)
	format := "parse error near %v (line %v symbol %v - line %v symbol %v):\n%v\n"
	if e.p.Pretty {
		format = "parse error near \x1B[34m%v\x1B[m (line %v symbol %v - line %v symbol %v):\n%v\n"
	}
	for _, token := range tokens {
		begin, end := int(token.begin), int(token.end)
		error += fmt.Sprintf(format,
			rul3s[token.pegRule],
			translations[begin].line, translations[begin].symbol,
			translations[end].line, translations[end].symbol,
			strconv.Quote(string(e.p.buffer[begin:end])))
	}

	return error
}

func (p *rangeQuery) PrintSyntaxTree() {
	if p.Pretty {
		p.tokens32.PrettyPrintSyntaxTree(p.Buffer)
	} else {
		p.tokens32.PrintSyntaxTree(p.Buffer)
	}
}

func (p *rangeQuery) Execute() {
	buffer, _buffer, text, begin, end := p.Buffer, p.buffer, "", 0, 0
	for _, token := range p.Tokens() {
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
	var (
		max                  token32
		position, tokenIndex uint32
		buffer               []rune
	)
	p.reset = func() {
		max = token32{}
		position, tokenIndex = 0, 0

		p.buffer = []rune(p.Buffer)
		if len(p.buffer) == 0 || p.buffer[len(p.buffer)-1] != endSymbol {
			p.buffer = append(p.buffer, endSymbol)
		}
		buffer = p.buffer
	}
	p.reset()

	_rules := p.rules
	tree := tokens32{tree: make([]token32, math.MaxInt16)}
	p.parse = func(rule ...int) error {
		r := 1
		if len(rule) > 0 {
			r = rule[0]
		}
		matches := p.rules[r]()
		p.tokens32 = tree
		if matches {
			p.Trim(tokenIndex)
			return nil
		}
		return &parseError{p, max}
	}

	add := func(rule pegRule, begin uint32) {
		tree.Add(rule, begin, position, tokenIndex)
		tokenIndex++
		if begin != position && position > max.end {
			max = token32{rule, begin, position}
		}
	}

	matchDot := func() bool {
		if buffer[position] != endSymbol {
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
			position0, tokenIndex0 := position, tokenIndex
			{
				position1 := position
				{
					position2, tokenIndex2 := position, tokenIndex
					if !_rules[rulecombinedexpr]() {
						goto l2
					}
					goto l3
				l2:
					position, tokenIndex = position2, tokenIndex2
				}
			l3:
				{
					position4, tokenIndex4 := position, tokenIndex
					if !matchDot() {
						goto l4
					}
					goto l0
				l4:
					position, tokenIndex = position4, tokenIndex4
				}
				add(ruleexpression, position1)
			}
			return true
		l0:
			position, tokenIndex = position0, tokenIndex0
			return false
		},
		/* 1 combinedexpr <- <(rangeexpr combinators?)> */
		func() bool {
			position5, tokenIndex5 := position, tokenIndex
			{
				position6 := position
				if !_rules[rulerangeexpr]() {
					goto l5
				}
				{
					position7, tokenIndex7 := position, tokenIndex
					if !_rules[rulecombinators]() {
						goto l7
					}
					goto l8
				l7:
					position, tokenIndex = position7, tokenIndex7
				}
			l8:
				add(rulecombinedexpr, position6)
			}
			return true
		l5:
			position, tokenIndex = position5, tokenIndex5
			return false
		},
		/* 2 rangeexpr <- <(space (const / function / cluster / clusterq / group / groupq / localkey / regex / value / brackets / (Action0 braces)) space)> */
		func() bool {
			position9, tokenIndex9 := position, tokenIndex
			{
				position10 := position
				if !_rules[rulespace]() {
					goto l9
				}
				{
					position11, tokenIndex11 := position, tokenIndex
					if !_rules[ruleconst]() {
						goto l12
					}
					goto l11
				l12:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[rulefunction]() {
						goto l13
					}
					goto l11
				l13:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[rulecluster]() {
						goto l14
					}
					goto l11
				l14:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[ruleclusterq]() {
						goto l15
					}
					goto l11
				l15:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[rulegroup]() {
						goto l16
					}
					goto l11
				l16:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[rulegroupq]() {
						goto l17
					}
					goto l11
				l17:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[rulelocalkey]() {
						goto l18
					}
					goto l11
				l18:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[ruleregex]() {
						goto l19
					}
					goto l11
				l19:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[rulevalue]() {
						goto l20
					}
					goto l11
				l20:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[rulebrackets]() {
						goto l21
					}
					goto l11
				l21:
					position, tokenIndex = position11, tokenIndex11
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
				add(rulerangeexpr, position10)
			}
			return true
		l9:
			position, tokenIndex = position9, tokenIndex9
			return false
		},
		/* 3 combinators <- <(space (union / intersect / exclude / braces))> */
		func() bool {
			position22, tokenIndex22 := position, tokenIndex
			{
				position23 := position
				if !_rules[rulespace]() {
					goto l22
				}
				{
					position24, tokenIndex24 := position, tokenIndex
					if !_rules[ruleunion]() {
						goto l25
					}
					goto l24
				l25:
					position, tokenIndex = position24, tokenIndex24
					if !_rules[ruleintersect]() {
						goto l26
					}
					goto l24
				l26:
					position, tokenIndex = position24, tokenIndex24
					if !_rules[ruleexclude]() {
						goto l27
					}
					goto l24
				l27:
					position, tokenIndex = position24, tokenIndex24
					if !_rules[rulebraces]() {
						goto l22
					}
				}
			l24:
				add(rulecombinators, position23)
			}
			return true
		l22:
			position, tokenIndex = position22, tokenIndex22
			return false
		},
		/* 4 intersect <- <('&' rangeexpr Action1 combinators?)> */
		func() bool {
			position28, tokenIndex28 := position, tokenIndex
			{
				position29 := position
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
					position30, tokenIndex30 := position, tokenIndex
					if !_rules[rulecombinators]() {
						goto l30
					}
					goto l31
				l30:
					position, tokenIndex = position30, tokenIndex30
				}
			l31:
				add(ruleintersect, position29)
			}
			return true
		l28:
			position, tokenIndex = position28, tokenIndex28
			return false
		},
		/* 5 exclude <- <('-' rangeexpr Action2 combinators?)> */
		func() bool {
			position32, tokenIndex32 := position, tokenIndex
			{
				position33 := position
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
					position34, tokenIndex34 := position, tokenIndex
					if !_rules[rulecombinators]() {
						goto l34
					}
					goto l35
				l34:
					position, tokenIndex = position34, tokenIndex34
				}
			l35:
				add(ruleexclude, position33)
			}
			return true
		l32:
			position, tokenIndex = position32, tokenIndex32
			return false
		},
		/* 6 union <- <(',' rangeexpr Action3 combinators?)> */
		func() bool {
			position36, tokenIndex36 := position, tokenIndex
			{
				position37 := position
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
					position38, tokenIndex38 := position, tokenIndex
					if !_rules[rulecombinators]() {
						goto l38
					}
					goto l39
				l38:
					position, tokenIndex = position38, tokenIndex38
				}
			l39:
				add(ruleunion, position37)
			}
			return true
		l36:
			position, tokenIndex = position36, tokenIndex36
			return false
		},
		/* 7 braces <- <('{' combinedexpr? '}' rangeexpr? Action4)> */
		func() bool {
			position40, tokenIndex40 := position, tokenIndex
			{
				position41 := position
				if buffer[position] != rune('{') {
					goto l40
				}
				position++
				{
					position42, tokenIndex42 := position, tokenIndex
					if !_rules[rulecombinedexpr]() {
						goto l42
					}
					goto l43
				l42:
					position, tokenIndex = position42, tokenIndex42
				}
			l43:
				if buffer[position] != rune('}') {
					goto l40
				}
				position++
				{
					position44, tokenIndex44 := position, tokenIndex
					if !_rules[rulerangeexpr]() {
						goto l44
					}
					goto l45
				l44:
					position, tokenIndex = position44, tokenIndex44
				}
			l45:
				if !_rules[ruleAction4]() {
					goto l40
				}
				add(rulebraces, position41)
			}
			return true
		l40:
			position, tokenIndex = position40, tokenIndex40
			return false
		},
		/* 8 brackets <- <('(' combinedexpr? ')')> */
		func() bool {
			position46, tokenIndex46 := position, tokenIndex
			{
				position47 := position
				if buffer[position] != rune('(') {
					goto l46
				}
				position++
				{
					position48, tokenIndex48 := position, tokenIndex
					if !_rules[rulecombinedexpr]() {
						goto l48
					}
					goto l49
				l48:
					position, tokenIndex = position48, tokenIndex48
				}
			l49:
				if buffer[position] != rune(')') {
					goto l46
				}
				position++
				add(rulebrackets, position47)
			}
			return true
		l46:
			position, tokenIndex = position46, tokenIndex46
			return false
		},
		/* 9 clusterq <- <('*' rangeexpr Action5)> */
		func() bool {
			position50, tokenIndex50 := position, tokenIndex
			{
				position51 := position
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
				add(ruleclusterq, position51)
			}
			return true
		l50:
			position, tokenIndex = position50, tokenIndex50
			return false
		},
		/* 10 groupq <- <('?' rangeexpr Action6)> */
		func() bool {
			position52, tokenIndex52 := position, tokenIndex
			{
				position53 := position
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
				add(rulegroupq, position53)
			}
			return true
		l52:
			position, tokenIndex = position52, tokenIndex52
			return false
		},
		/* 11 cluster <- <(('%' literal Action7 key?) / ('%' rangeexpr Action8 key?))> */
		func() bool {
			position54, tokenIndex54 := position, tokenIndex
			{
				position55 := position
				{
					position56, tokenIndex56 := position, tokenIndex
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
						position58, tokenIndex58 := position, tokenIndex
						if !_rules[rulekey]() {
							goto l58
						}
						goto l59
					l58:
						position, tokenIndex = position58, tokenIndex58
					}
				l59:
					goto l56
				l57:
					position, tokenIndex = position56, tokenIndex56
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
						position60, tokenIndex60 := position, tokenIndex
						if !_rules[rulekey]() {
							goto l60
						}
						goto l61
					l60:
						position, tokenIndex = position60, tokenIndex60
					}
				l61:
				}
			l56:
				add(rulecluster, position55)
			}
			return true
		l54:
			position, tokenIndex = position54, tokenIndex54
			return false
		},
		/* 12 group <- <('@' rangeexpr Action9)> */
		func() bool {
			position62, tokenIndex62 := position, tokenIndex
			{
				position63 := position
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
				add(rulegroup, position63)
			}
			return true
		l62:
			position, tokenIndex = position62, tokenIndex62
			return false
		},
		/* 13 key <- <(':' rangeexpr Action10)> */
		func() bool {
			position64, tokenIndex64 := position, tokenIndex
			{
				position65 := position
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
				add(rulekey, position65)
			}
			return true
		l64:
			position, tokenIndex = position64, tokenIndex64
			return false
		},
		/* 14 localkey <- <('$' rangeexpr Action11)> */
		func() bool {
			position66, tokenIndex66 := position, tokenIndex
			{
				position67 := position
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
				add(rulelocalkey, position67)
			}
			return true
		l66:
			position, tokenIndex = position66, tokenIndex66
			return false
		},
		/* 15 function <- <(literal Action12 '(' funcargs ')')> */
		func() bool {
			position68, tokenIndex68 := position, tokenIndex
			{
				position69 := position
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
				add(rulefunction, position69)
			}
			return true
		l68:
			position, tokenIndex = position68, tokenIndex68
			return false
		},
		/* 16 funcargs <- <((combinedexpr? Action13 ';' funcargs) / (combinedexpr? Action14))> */
		func() bool {
			position70, tokenIndex70 := position, tokenIndex
			{
				position71 := position
				{
					position72, tokenIndex72 := position, tokenIndex
					{
						position74, tokenIndex74 := position, tokenIndex
						if !_rules[rulecombinedexpr]() {
							goto l74
						}
						goto l75
					l74:
						position, tokenIndex = position74, tokenIndex74
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
					position, tokenIndex = position72, tokenIndex72
					{
						position76, tokenIndex76 := position, tokenIndex
						if !_rules[rulecombinedexpr]() {
							goto l76
						}
						goto l77
					l76:
						position, tokenIndex = position76, tokenIndex76
					}
				l77:
					if !_rules[ruleAction14]() {
						goto l70
					}
				}
			l72:
				add(rulefuncargs, position71)
			}
			return true
		l70:
			position, tokenIndex = position70, tokenIndex70
			return false
		},
		/* 17 regex <- <('/' <(!'/' .)*> '/' Action15)> */
		func() bool {
			position78, tokenIndex78 := position, tokenIndex
			{
				position79 := position
				if buffer[position] != rune('/') {
					goto l78
				}
				position++
				{
					position80 := position
				l81:
					{
						position82, tokenIndex82 := position, tokenIndex
						{
							position83, tokenIndex83 := position, tokenIndex
							if buffer[position] != rune('/') {
								goto l83
							}
							position++
							goto l82
						l83:
							position, tokenIndex = position83, tokenIndex83
						}
						if !matchDot() {
							goto l82
						}
						goto l81
					l82:
						position, tokenIndex = position82, tokenIndex82
					}
					add(rulePegText, position80)
				}
				if buffer[position] != rune('/') {
					goto l78
				}
				position++
				if !_rules[ruleAction15]() {
					goto l78
				}
				add(ruleregex, position79)
			}
			return true
		l78:
			position, tokenIndex = position78, tokenIndex78
			return false
		},
		/* 18 literal <- <<(leaderChar ([a-z] / [A-Z] / ([0-9] / [0-9]) / '-' / '_')*)>> */
		func() bool {
			position84, tokenIndex84 := position, tokenIndex
			{
				position85 := position
				{
					position86 := position
					if !_rules[ruleleaderChar]() {
						goto l84
					}
				l87:
					{
						position88, tokenIndex88 := position, tokenIndex
						{
							position89, tokenIndex89 := position, tokenIndex
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l90
							}
							position++
							goto l89
						l90:
							position, tokenIndex = position89, tokenIndex89
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l91
							}
							position++
							goto l89
						l91:
							position, tokenIndex = position89, tokenIndex89
							{
								position93, tokenIndex93 := position, tokenIndex
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l94
								}
								position++
								goto l93
							l94:
								position, tokenIndex = position93, tokenIndex93
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l92
								}
								position++
							}
						l93:
							goto l89
						l92:
							position, tokenIndex = position89, tokenIndex89
							if buffer[position] != rune('-') {
								goto l95
							}
							position++
							goto l89
						l95:
							position, tokenIndex = position89, tokenIndex89
							if buffer[position] != rune('_') {
								goto l88
							}
							position++
						}
					l89:
						goto l87
					l88:
						position, tokenIndex = position88, tokenIndex88
					}
					add(rulePegText, position86)
				}
				add(ruleliteral, position85)
			}
			return true
		l84:
			position, tokenIndex = position84, tokenIndex84
			return false
		},
		/* 19 value <- <(<(leaderChar (':' / ([a-z] / [A-Z]) / ([0-9] / [0-9]) / '-' / '_' / '.')*)> Action16)> */
		func() bool {
			position96, tokenIndex96 := position, tokenIndex
			{
				position97 := position
				{
					position98 := position
					if !_rules[ruleleaderChar]() {
						goto l96
					}
				l99:
					{
						position100, tokenIndex100 := position, tokenIndex
						{
							position101, tokenIndex101 := position, tokenIndex
							if buffer[position] != rune(':') {
								goto l102
							}
							position++
							goto l101
						l102:
							position, tokenIndex = position101, tokenIndex101
							{
								position104, tokenIndex104 := position, tokenIndex
								if c := buffer[position]; c < rune('a') || c > rune('z') {
									goto l105
								}
								position++
								goto l104
							l105:
								position, tokenIndex = position104, tokenIndex104
								if c := buffer[position]; c < rune('A') || c > rune('Z') {
									goto l103
								}
								position++
							}
						l104:
							goto l101
						l103:
							position, tokenIndex = position101, tokenIndex101
							{
								position107, tokenIndex107 := position, tokenIndex
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l108
								}
								position++
								goto l107
							l108:
								position, tokenIndex = position107, tokenIndex107
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l106
								}
								position++
							}
						l107:
							goto l101
						l106:
							position, tokenIndex = position101, tokenIndex101
							if buffer[position] != rune('-') {
								goto l109
							}
							position++
							goto l101
						l109:
							position, tokenIndex = position101, tokenIndex101
							if buffer[position] != rune('_') {
								goto l110
							}
							position++
							goto l101
						l110:
							position, tokenIndex = position101, tokenIndex101
							if buffer[position] != rune('.') {
								goto l100
							}
							position++
						}
					l101:
						goto l99
					l100:
						position, tokenIndex = position100, tokenIndex100
					}
					add(rulePegText, position98)
				}
				if !_rules[ruleAction16]() {
					goto l96
				}
				add(rulevalue, position97)
			}
			return true
		l96:
			position, tokenIndex = position96, tokenIndex96
			return false
		},
		/* 20 leaderChar <- <([a-z] / [A-Z] / ([0-9] / [0-9]) / '.' / '_')> */
		func() bool {
			position111, tokenIndex111 := position, tokenIndex
			{
				position112 := position
				{
					position113, tokenIndex113 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l114
					}
					position++
					goto l113
				l114:
					position, tokenIndex = position113, tokenIndex113
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l115
					}
					position++
					goto l113
				l115:
					position, tokenIndex = position113, tokenIndex113
					{
						position117, tokenIndex117 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l118
						}
						position++
						goto l117
					l118:
						position, tokenIndex = position117, tokenIndex117
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l116
						}
						position++
					}
				l117:
					goto l113
				l116:
					position, tokenIndex = position113, tokenIndex113
					if buffer[position] != rune('.') {
						goto l119
					}
					position++
					goto l113
				l119:
					position, tokenIndex = position113, tokenIndex113
					if buffer[position] != rune('_') {
						goto l111
					}
					position++
				}
			l113:
				add(ruleleaderChar, position112)
			}
			return true
		l111:
			position, tokenIndex = position111, tokenIndex111
			return false
		},
		/* 21 space <- <' '*> */
		func() bool {
			{
				position121 := position
			l122:
				{
					position123, tokenIndex123 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l123
					}
					position++
					goto l122
				l123:
					position, tokenIndex = position123, tokenIndex123
				}
				add(rulespace, position121)
			}
			return true
		},
		/* 22 const <- <(q / quoted)> */
		func() bool {
			position124, tokenIndex124 := position, tokenIndex
			{
				position125 := position
				{
					position126, tokenIndex126 := position, tokenIndex
					if !_rules[ruleq]() {
						goto l127
					}
					goto l126
				l127:
					position, tokenIndex = position126, tokenIndex126
					if !_rules[rulequoted]() {
						goto l124
					}
				}
			l126:
				add(ruleconst, position125)
			}
			return true
		l124:
			position, tokenIndex = position124, tokenIndex124
			return false
		},
		/* 23 q <- <('q' '(' <(!')' .)*> ')' Action17)> */
		func() bool {
			position128, tokenIndex128 := position, tokenIndex
			{
				position129 := position
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
				l131:
					{
						position132, tokenIndex132 := position, tokenIndex
						{
							position133, tokenIndex133 := position, tokenIndex
							if buffer[position] != rune(')') {
								goto l133
							}
							position++
							goto l132
						l133:
							position, tokenIndex = position133, tokenIndex133
						}
						if !matchDot() {
							goto l132
						}
						goto l131
					l132:
						position, tokenIndex = position132, tokenIndex132
					}
					add(rulePegText, position130)
				}
				if buffer[position] != rune(')') {
					goto l128
				}
				position++
				if !_rules[ruleAction17]() {
					goto l128
				}
				add(ruleq, position129)
			}
			return true
		l128:
			position, tokenIndex = position128, tokenIndex128
			return false
		},
		/* 24 quoted <- <('"' <(!'"' .)*> '"' Action18)> */
		func() bool {
			position134, tokenIndex134 := position, tokenIndex
			{
				position135 := position
				if buffer[position] != rune('"') {
					goto l134
				}
				position++
				{
					position136 := position
				l137:
					{
						position138, tokenIndex138 := position, tokenIndex
						{
							position139, tokenIndex139 := position, tokenIndex
							if buffer[position] != rune('"') {
								goto l139
							}
							position++
							goto l138
						l139:
							position, tokenIndex = position139, tokenIndex139
						}
						if !matchDot() {
							goto l138
						}
						goto l137
					l138:
						position, tokenIndex = position138, tokenIndex138
					}
					add(rulePegText, position136)
				}
				if buffer[position] != rune('"') {
					goto l134
				}
				position++
				if !_rules[ruleAction18]() {
					goto l134
				}
				add(rulequoted, position135)
			}
			return true
		l134:
			position, tokenIndex = position134, tokenIndex134
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
