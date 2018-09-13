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
	ruleemptybraces
	ruleemptybrackets
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
	ruleAction15
	ruleAction16
	rulePegText
	ruleAction17
	ruleAction18
	ruleAction19
	ruleAction20
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
	"emptybraces",
	"emptybrackets",
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
	"Action15",
	"Action16",
	"PegText",
	"Action17",
	"Action18",
	"Action19",
	"Action20",
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
	rules  [50]func() bool
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
			p.pushNode(nodeNull{})
		case ruleAction5:
			p.pushNode(nodeNull{})
		case ruleAction6:
			p.addBraces()
		case ruleAction7:
			p.addClusterQuery()
		case ruleAction8:
			p.addGroupQuery()
		case ruleAction9:
			p.addValue(buffer[begin:end])
			p.addClusterLookup()
		case ruleAction10:
			p.addClusterLookup()
		case ruleAction11:
			p.addGroupLookup()
		case ruleAction12:
			p.addKeyLookup()
		case ruleAction13:
			p.addLocalClusterLookup()
		case ruleAction14:
			p.addFunction(buffer[begin:end])
		case ruleAction15:
			p.addFuncArg()
		case ruleAction16:
			p.addFuncArg()
		case ruleAction17:
			p.addRegex(buffer[begin:end])
		case ruleAction18:
			p.addValue(buffer[begin:end])
		case ruleAction19:
			p.addConstant(buffer[begin:end])
		case ruleAction20:
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
		/* 2 rangeexpr <- <(space (const / function / cluster / clusterq / group / groupq / localkey / regex / value / emptybrackets / brackets / emptybraces / (Action0 braces)) space)> */
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
					if !_rules[ruleemptybrackets]() {
						goto l21
					}
					goto l11
				l21:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[rulebrackets]() {
						goto l22
					}
					goto l11
				l22:
					position, tokenIndex = position11, tokenIndex11
					if !_rules[ruleemptybraces]() {
						goto l23
					}
					goto l11
				l23:
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
			position24, tokenIndex24 := position, tokenIndex
			{
				position25 := position
				if !_rules[rulespace]() {
					goto l24
				}
				{
					position26, tokenIndex26 := position, tokenIndex
					if !_rules[ruleunion]() {
						goto l27
					}
					goto l26
				l27:
					position, tokenIndex = position26, tokenIndex26
					if !_rules[ruleintersect]() {
						goto l28
					}
					goto l26
				l28:
					position, tokenIndex = position26, tokenIndex26
					if !_rules[ruleexclude]() {
						goto l29
					}
					goto l26
				l29:
					position, tokenIndex = position26, tokenIndex26
					if !_rules[rulebraces]() {
						goto l24
					}
				}
			l26:
				add(rulecombinators, position25)
			}
			return true
		l24:
			position, tokenIndex = position24, tokenIndex24
			return false
		},
		/* 4 intersect <- <('&' rangeexpr Action1 combinators?)> */
		func() bool {
			position30, tokenIndex30 := position, tokenIndex
			{
				position31 := position
				if buffer[position] != rune('&') {
					goto l30
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l30
				}
				if !_rules[ruleAction1]() {
					goto l30
				}
				{
					position32, tokenIndex32 := position, tokenIndex
					if !_rules[rulecombinators]() {
						goto l32
					}
					goto l33
				l32:
					position, tokenIndex = position32, tokenIndex32
				}
			l33:
				add(ruleintersect, position31)
			}
			return true
		l30:
			position, tokenIndex = position30, tokenIndex30
			return false
		},
		/* 5 exclude <- <('-' rangeexpr Action2 combinators?)> */
		func() bool {
			position34, tokenIndex34 := position, tokenIndex
			{
				position35 := position
				if buffer[position] != rune('-') {
					goto l34
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l34
				}
				if !_rules[ruleAction2]() {
					goto l34
				}
				{
					position36, tokenIndex36 := position, tokenIndex
					if !_rules[rulecombinators]() {
						goto l36
					}
					goto l37
				l36:
					position, tokenIndex = position36, tokenIndex36
				}
			l37:
				add(ruleexclude, position35)
			}
			return true
		l34:
			position, tokenIndex = position34, tokenIndex34
			return false
		},
		/* 6 union <- <(',' rangeexpr Action3 combinators?)> */
		func() bool {
			position38, tokenIndex38 := position, tokenIndex
			{
				position39 := position
				if buffer[position] != rune(',') {
					goto l38
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l38
				}
				if !_rules[ruleAction3]() {
					goto l38
				}
				{
					position40, tokenIndex40 := position, tokenIndex
					if !_rules[rulecombinators]() {
						goto l40
					}
					goto l41
				l40:
					position, tokenIndex = position40, tokenIndex40
				}
			l41:
				add(ruleunion, position39)
			}
			return true
		l38:
			position, tokenIndex = position38, tokenIndex38
			return false
		},
		/* 7 emptybraces <- <('{' space '}' Action4)> */
		func() bool {
			position42, tokenIndex42 := position, tokenIndex
			{
				position43 := position
				if buffer[position] != rune('{') {
					goto l42
				}
				position++
				if !_rules[rulespace]() {
					goto l42
				}
				if buffer[position] != rune('}') {
					goto l42
				}
				position++
				if !_rules[ruleAction4]() {
					goto l42
				}
				add(ruleemptybraces, position43)
			}
			return true
		l42:
			position, tokenIndex = position42, tokenIndex42
			return false
		},
		/* 8 emptybrackets <- <('(' space ')' Action5)> */
		func() bool {
			position44, tokenIndex44 := position, tokenIndex
			{
				position45 := position
				if buffer[position] != rune('(') {
					goto l44
				}
				position++
				if !_rules[rulespace]() {
					goto l44
				}
				if buffer[position] != rune(')') {
					goto l44
				}
				position++
				if !_rules[ruleAction5]() {
					goto l44
				}
				add(ruleemptybrackets, position45)
			}
			return true
		l44:
			position, tokenIndex = position44, tokenIndex44
			return false
		},
		/* 9 braces <- <('{' combinedexpr? '}' rangeexpr? Action6)> */
		func() bool {
			position46, tokenIndex46 := position, tokenIndex
			{
				position47 := position
				if buffer[position] != rune('{') {
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
				if buffer[position] != rune('}') {
					goto l46
				}
				position++
				{
					position50, tokenIndex50 := position, tokenIndex
					if !_rules[rulerangeexpr]() {
						goto l50
					}
					goto l51
				l50:
					position, tokenIndex = position50, tokenIndex50
				}
			l51:
				if !_rules[ruleAction6]() {
					goto l46
				}
				add(rulebraces, position47)
			}
			return true
		l46:
			position, tokenIndex = position46, tokenIndex46
			return false
		},
		/* 10 brackets <- <('(' combinedexpr? ')')> */
		func() bool {
			position52, tokenIndex52 := position, tokenIndex
			{
				position53 := position
				if buffer[position] != rune('(') {
					goto l52
				}
				position++
				{
					position54, tokenIndex54 := position, tokenIndex
					if !_rules[rulecombinedexpr]() {
						goto l54
					}
					goto l55
				l54:
					position, tokenIndex = position54, tokenIndex54
				}
			l55:
				if buffer[position] != rune(')') {
					goto l52
				}
				position++
				add(rulebrackets, position53)
			}
			return true
		l52:
			position, tokenIndex = position52, tokenIndex52
			return false
		},
		/* 11 clusterq <- <('*' rangeexpr Action7)> */
		func() bool {
			position56, tokenIndex56 := position, tokenIndex
			{
				position57 := position
				if buffer[position] != rune('*') {
					goto l56
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l56
				}
				if !_rules[ruleAction7]() {
					goto l56
				}
				add(ruleclusterq, position57)
			}
			return true
		l56:
			position, tokenIndex = position56, tokenIndex56
			return false
		},
		/* 12 groupq <- <('?' rangeexpr Action8)> */
		func() bool {
			position58, tokenIndex58 := position, tokenIndex
			{
				position59 := position
				if buffer[position] != rune('?') {
					goto l58
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l58
				}
				if !_rules[ruleAction8]() {
					goto l58
				}
				add(rulegroupq, position59)
			}
			return true
		l58:
			position, tokenIndex = position58, tokenIndex58
			return false
		},
		/* 13 cluster <- <(('%' literal Action9 key?) / ('%' rangeexpr Action10 key?))> */
		func() bool {
			position60, tokenIndex60 := position, tokenIndex
			{
				position61 := position
				{
					position62, tokenIndex62 := position, tokenIndex
					if buffer[position] != rune('%') {
						goto l63
					}
					position++
					if !_rules[ruleliteral]() {
						goto l63
					}
					if !_rules[ruleAction9]() {
						goto l63
					}
					{
						position64, tokenIndex64 := position, tokenIndex
						if !_rules[rulekey]() {
							goto l64
						}
						goto l65
					l64:
						position, tokenIndex = position64, tokenIndex64
					}
				l65:
					goto l62
				l63:
					position, tokenIndex = position62, tokenIndex62
					if buffer[position] != rune('%') {
						goto l60
					}
					position++
					if !_rules[rulerangeexpr]() {
						goto l60
					}
					if !_rules[ruleAction10]() {
						goto l60
					}
					{
						position66, tokenIndex66 := position, tokenIndex
						if !_rules[rulekey]() {
							goto l66
						}
						goto l67
					l66:
						position, tokenIndex = position66, tokenIndex66
					}
				l67:
				}
			l62:
				add(rulecluster, position61)
			}
			return true
		l60:
			position, tokenIndex = position60, tokenIndex60
			return false
		},
		/* 14 group <- <('@' rangeexpr Action11)> */
		func() bool {
			position68, tokenIndex68 := position, tokenIndex
			{
				position69 := position
				if buffer[position] != rune('@') {
					goto l68
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l68
				}
				if !_rules[ruleAction11]() {
					goto l68
				}
				add(rulegroup, position69)
			}
			return true
		l68:
			position, tokenIndex = position68, tokenIndex68
			return false
		},
		/* 15 key <- <(':' rangeexpr Action12)> */
		func() bool {
			position70, tokenIndex70 := position, tokenIndex
			{
				position71 := position
				if buffer[position] != rune(':') {
					goto l70
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l70
				}
				if !_rules[ruleAction12]() {
					goto l70
				}
				add(rulekey, position71)
			}
			return true
		l70:
			position, tokenIndex = position70, tokenIndex70
			return false
		},
		/* 16 localkey <- <('$' rangeexpr Action13)> */
		func() bool {
			position72, tokenIndex72 := position, tokenIndex
			{
				position73 := position
				if buffer[position] != rune('$') {
					goto l72
				}
				position++
				if !_rules[rulerangeexpr]() {
					goto l72
				}
				if !_rules[ruleAction13]() {
					goto l72
				}
				add(rulelocalkey, position73)
			}
			return true
		l72:
			position, tokenIndex = position72, tokenIndex72
			return false
		},
		/* 17 function <- <(literal Action14 '(' funcargs ')')> */
		func() bool {
			position74, tokenIndex74 := position, tokenIndex
			{
				position75 := position
				if !_rules[ruleliteral]() {
					goto l74
				}
				if !_rules[ruleAction14]() {
					goto l74
				}
				if buffer[position] != rune('(') {
					goto l74
				}
				position++
				if !_rules[rulefuncargs]() {
					goto l74
				}
				if buffer[position] != rune(')') {
					goto l74
				}
				position++
				add(rulefunction, position75)
			}
			return true
		l74:
			position, tokenIndex = position74, tokenIndex74
			return false
		},
		/* 18 funcargs <- <((combinedexpr? Action15 ';' funcargs) / (combinedexpr? Action16))> */
		func() bool {
			position76, tokenIndex76 := position, tokenIndex
			{
				position77 := position
				{
					position78, tokenIndex78 := position, tokenIndex
					{
						position80, tokenIndex80 := position, tokenIndex
						if !_rules[rulecombinedexpr]() {
							goto l80
						}
						goto l81
					l80:
						position, tokenIndex = position80, tokenIndex80
					}
				l81:
					if !_rules[ruleAction15]() {
						goto l79
					}
					if buffer[position] != rune(';') {
						goto l79
					}
					position++
					if !_rules[rulefuncargs]() {
						goto l79
					}
					goto l78
				l79:
					position, tokenIndex = position78, tokenIndex78
					{
						position82, tokenIndex82 := position, tokenIndex
						if !_rules[rulecombinedexpr]() {
							goto l82
						}
						goto l83
					l82:
						position, tokenIndex = position82, tokenIndex82
					}
				l83:
					if !_rules[ruleAction16]() {
						goto l76
					}
				}
			l78:
				add(rulefuncargs, position77)
			}
			return true
		l76:
			position, tokenIndex = position76, tokenIndex76
			return false
		},
		/* 19 regex <- <('/' <(!'/' .)*> '/' Action17)> */
		func() bool {
			position84, tokenIndex84 := position, tokenIndex
			{
				position85 := position
				if buffer[position] != rune('/') {
					goto l84
				}
				position++
				{
					position86 := position
				l87:
					{
						position88, tokenIndex88 := position, tokenIndex
						{
							position89, tokenIndex89 := position, tokenIndex
							if buffer[position] != rune('/') {
								goto l89
							}
							position++
							goto l88
						l89:
							position, tokenIndex = position89, tokenIndex89
						}
						if !matchDot() {
							goto l88
						}
						goto l87
					l88:
						position, tokenIndex = position88, tokenIndex88
					}
					add(rulePegText, position86)
				}
				if buffer[position] != rune('/') {
					goto l84
				}
				position++
				if !_rules[ruleAction17]() {
					goto l84
				}
				add(ruleregex, position85)
			}
			return true
		l84:
			position, tokenIndex = position84, tokenIndex84
			return false
		},
		/* 20 literal <- <<(leaderChar ([a-z] / [A-Z] / ([0-9] / [0-9]) / '-' / '_' / '.')*)>> */
		func() bool {
			position90, tokenIndex90 := position, tokenIndex
			{
				position91 := position
				{
					position92 := position
					if !_rules[ruleleaderChar]() {
						goto l90
					}
				l93:
					{
						position94, tokenIndex94 := position, tokenIndex
						{
							position95, tokenIndex95 := position, tokenIndex
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l96
							}
							position++
							goto l95
						l96:
							position, tokenIndex = position95, tokenIndex95
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l97
							}
							position++
							goto l95
						l97:
							position, tokenIndex = position95, tokenIndex95
							{
								position99, tokenIndex99 := position, tokenIndex
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l100
								}
								position++
								goto l99
							l100:
								position, tokenIndex = position99, tokenIndex99
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l98
								}
								position++
							}
						l99:
							goto l95
						l98:
							position, tokenIndex = position95, tokenIndex95
							if buffer[position] != rune('-') {
								goto l101
							}
							position++
							goto l95
						l101:
							position, tokenIndex = position95, tokenIndex95
							if buffer[position] != rune('_') {
								goto l102
							}
							position++
							goto l95
						l102:
							position, tokenIndex = position95, tokenIndex95
							if buffer[position] != rune('.') {
								goto l94
							}
							position++
						}
					l95:
						goto l93
					l94:
						position, tokenIndex = position94, tokenIndex94
					}
					add(rulePegText, position92)
				}
				add(ruleliteral, position91)
			}
			return true
		l90:
			position, tokenIndex = position90, tokenIndex90
			return false
		},
		/* 21 value <- <(<(leaderChar (':' / ([a-z] / [A-Z]) / ([0-9] / [0-9]) / '-' / '_' / '.')*)> Action18)> */
		func() bool {
			position103, tokenIndex103 := position, tokenIndex
			{
				position104 := position
				{
					position105 := position
					if !_rules[ruleleaderChar]() {
						goto l103
					}
				l106:
					{
						position107, tokenIndex107 := position, tokenIndex
						{
							position108, tokenIndex108 := position, tokenIndex
							if buffer[position] != rune(':') {
								goto l109
							}
							position++
							goto l108
						l109:
							position, tokenIndex = position108, tokenIndex108
							{
								position111, tokenIndex111 := position, tokenIndex
								if c := buffer[position]; c < rune('a') || c > rune('z') {
									goto l112
								}
								position++
								goto l111
							l112:
								position, tokenIndex = position111, tokenIndex111
								if c := buffer[position]; c < rune('A') || c > rune('Z') {
									goto l110
								}
								position++
							}
						l111:
							goto l108
						l110:
							position, tokenIndex = position108, tokenIndex108
							{
								position114, tokenIndex114 := position, tokenIndex
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l115
								}
								position++
								goto l114
							l115:
								position, tokenIndex = position114, tokenIndex114
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l113
								}
								position++
							}
						l114:
							goto l108
						l113:
							position, tokenIndex = position108, tokenIndex108
							if buffer[position] != rune('-') {
								goto l116
							}
							position++
							goto l108
						l116:
							position, tokenIndex = position108, tokenIndex108
							if buffer[position] != rune('_') {
								goto l117
							}
							position++
							goto l108
						l117:
							position, tokenIndex = position108, tokenIndex108
							if buffer[position] != rune('.') {
								goto l107
							}
							position++
						}
					l108:
						goto l106
					l107:
						position, tokenIndex = position107, tokenIndex107
					}
					add(rulePegText, position105)
				}
				if !_rules[ruleAction18]() {
					goto l103
				}
				add(rulevalue, position104)
			}
			return true
		l103:
			position, tokenIndex = position103, tokenIndex103
			return false
		},
		/* 22 leaderChar <- <([a-z] / [A-Z] / ([0-9] / [0-9]) / '.' / '_')> */
		func() bool {
			position118, tokenIndex118 := position, tokenIndex
			{
				position119 := position
				{
					position120, tokenIndex120 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l121
					}
					position++
					goto l120
				l121:
					position, tokenIndex = position120, tokenIndex120
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l122
					}
					position++
					goto l120
				l122:
					position, tokenIndex = position120, tokenIndex120
					{
						position124, tokenIndex124 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l125
						}
						position++
						goto l124
					l125:
						position, tokenIndex = position124, tokenIndex124
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l123
						}
						position++
					}
				l124:
					goto l120
				l123:
					position, tokenIndex = position120, tokenIndex120
					if buffer[position] != rune('.') {
						goto l126
					}
					position++
					goto l120
				l126:
					position, tokenIndex = position120, tokenIndex120
					if buffer[position] != rune('_') {
						goto l118
					}
					position++
				}
			l120:
				add(ruleleaderChar, position119)
			}
			return true
		l118:
			position, tokenIndex = position118, tokenIndex118
			return false
		},
		/* 23 space <- <' '*> */
		func() bool {
			{
				position128 := position
			l129:
				{
					position130, tokenIndex130 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l130
					}
					position++
					goto l129
				l130:
					position, tokenIndex = position130, tokenIndex130
				}
				add(rulespace, position128)
			}
			return true
		},
		/* 24 const <- <(q / quoted)> */
		func() bool {
			position131, tokenIndex131 := position, tokenIndex
			{
				position132 := position
				{
					position133, tokenIndex133 := position, tokenIndex
					if !_rules[ruleq]() {
						goto l134
					}
					goto l133
				l134:
					position, tokenIndex = position133, tokenIndex133
					if !_rules[rulequoted]() {
						goto l131
					}
				}
			l133:
				add(ruleconst, position132)
			}
			return true
		l131:
			position, tokenIndex = position131, tokenIndex131
			return false
		},
		/* 25 q <- <('q' '(' <(!')' .)*> ')' Action19)> */
		func() bool {
			position135, tokenIndex135 := position, tokenIndex
			{
				position136 := position
				if buffer[position] != rune('q') {
					goto l135
				}
				position++
				if buffer[position] != rune('(') {
					goto l135
				}
				position++
				{
					position137 := position
				l138:
					{
						position139, tokenIndex139 := position, tokenIndex
						{
							position140, tokenIndex140 := position, tokenIndex
							if buffer[position] != rune(')') {
								goto l140
							}
							position++
							goto l139
						l140:
							position, tokenIndex = position140, tokenIndex140
						}
						if !matchDot() {
							goto l139
						}
						goto l138
					l139:
						position, tokenIndex = position139, tokenIndex139
					}
					add(rulePegText, position137)
				}
				if buffer[position] != rune(')') {
					goto l135
				}
				position++
				if !_rules[ruleAction19]() {
					goto l135
				}
				add(ruleq, position136)
			}
			return true
		l135:
			position, tokenIndex = position135, tokenIndex135
			return false
		},
		/* 26 quoted <- <('"' <(!'"' .)*> '"' Action20)> */
		func() bool {
			position141, tokenIndex141 := position, tokenIndex
			{
				position142 := position
				if buffer[position] != rune('"') {
					goto l141
				}
				position++
				{
					position143 := position
				l144:
					{
						position145, tokenIndex145 := position, tokenIndex
						{
							position146, tokenIndex146 := position, tokenIndex
							if buffer[position] != rune('"') {
								goto l146
							}
							position++
							goto l145
						l146:
							position, tokenIndex = position146, tokenIndex146
						}
						if !matchDot() {
							goto l145
						}
						goto l144
					l145:
						position, tokenIndex = position145, tokenIndex145
					}
					add(rulePegText, position143)
				}
				if buffer[position] != rune('"') {
					goto l141
				}
				position++
				if !_rules[ruleAction20]() {
					goto l141
				}
				add(rulequoted, position142)
			}
			return true
		l141:
			position, tokenIndex = position141, tokenIndex141
			return false
		},
		/* 28 Action0 <- <{ p.addBraceStart() }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 29 Action1 <- <{ p.addOperator(operatorIntersect) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 30 Action2 <- <{ p.addOperator(operatorSubtract) }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 31 Action3 <- <{ p.addOperator(operatorUnion) }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 32 Action4 <- <{ p.pushNode(nodeNull{}) }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 33 Action5 <- <{ p.pushNode(nodeNull{}) }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 34 Action6 <- <{ p.addBraces() }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 35 Action7 <- <{ p.addClusterQuery() }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 36 Action8 <- <{ p.addGroupQuery() }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 37 Action9 <- <{ p.addValue(buffer[begin:end]); p.addClusterLookup() }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 38 Action10 <- <{ p.addClusterLookup() }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 39 Action11 <- <{ p.addGroupLookup() }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		/* 40 Action12 <- <{ p.addKeyLookup() }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		/* 41 Action13 <- <{ p.addLocalClusterLookup() }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 42 Action14 <- <{ p.addFunction(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		/* 43 Action15 <- <{ p.addFuncArg() }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
		/* 44 Action16 <- <{ p.addFuncArg() }> */
		func() bool {
			{
				add(ruleAction16, position)
			}
			return true
		},
		nil,
		/* 46 Action17 <- <{ p.addRegex(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction17, position)
			}
			return true
		},
		/* 47 Action18 <- <{ p.addValue(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction18, position)
			}
			return true
		},
		/* 48 Action19 <- <{ p.addConstant(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction19, position)
			}
			return true
		},
		/* 49 Action20 <- <{ p.addConstant(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction20, position)
			}
			return true
		},
	}
	p.rules = _rules
}
