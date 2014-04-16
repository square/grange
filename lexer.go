package grange

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// A lexer of the form described in https://www.youtube.com/watch?v=HxaD_trXwRE
// State is represented with a function pointer, items are emitted on a
// channel.
func lexRange(name, input string) (*lexer, chan item) {
	l := &lexer{
		name:       name,
		input:      input,
		items:      make(chan item),
		commaStack: 0,
		inSubexpr:  false,
	}
	go l.run()
	return l, l.items
}

func (l *lexer) run() {
	for state := lexText; state != nil; {
		state = state(l)
	}
	close(l.items)
}

// ------------
// Range syntax
// ------------

// Base state for the lexer
func lexText(l *lexer) stateFn {
	punctuation := map[string]itemType{}
	punctuation["&"] = itemIntersect
	punctuation["-"] = itemExclude
	punctuation[","] = itemComma

	for {
		if strings.HasPrefix(l.input[l.pos:], "%{") {
			if l.inSubexpr {
				return l.errorf("May not nest subexpressions")
			}
			l.inSubexpr = true

			return lexConst("%{", itemSubexprStart)
		}
		if strings.HasPrefix(l.input[l.pos:], "{") {
			l.emitTextIfPresent()
			l.commaStack++
			return lexConst("{", itemLeftGroup)
		}
		if strings.HasPrefix(l.input[l.pos:], "}") {
			l.emitTextIfPresent()

			// if stack empty and in subexpression, it's subexpr end
			if l.inSubexpr && l.commaStack == 0 {
				l.inSubexpr = false
				return lexConst("}", itemSubexprEnd)
			}

			l.commaStack--
			return lexConst("}", itemRightGroup)
		}
		if strings.HasPrefix(l.input[l.pos:], "(") {
			return lexFunction
		}

		if strings.HasPrefix(l.input[l.pos:], "/") {
			return lexMatch
		}

		if strings.HasPrefix(l.input[l.pos:], "@") {
			return lexIdentifier("@", itemGroupLookup)
		}

		if strings.HasPrefix(l.input[l.pos:], "$") {
			return lexIdentifier("$", itemLocalClusterKey)
		}

		if strings.HasPrefix(l.input[l.pos:], ":") {
			return lexIdentifier(":", itemClusterKey)
		}

		if strings.HasPrefix(l.input[l.pos:], clusterMeta) {
			if l.pos > l.start {
				return l.errorf("preceeding chars: %s", l.input[l.start:l.pos])
			}
			return lexIdentifier(clusterMeta, itemCluster)
		}

		for str, i := range punctuation {
			if strings.HasPrefix(l.input[l.pos:], str) {
				l.emitTextIfPresent()

				return lexConst(str, i)
			}
		}

		if strings.HasPrefix(l.input[l.pos:], " ") {
			l.emitTextIfPresent()
			l.next()
			l.ignore()
			return lexText
		}

		if l.next() == eof {
			break
		}
	}
	l.emitTextIfPresent()
	l.emit(itemEOF)
	return nil
}

const clusterMeta = "%"
const clusterKeyMeta = ":"
const intersectMeta = "&"
const commaMeta = ","

func lexConst(str string, t itemType) stateFn {
	return func(l *lexer) stateFn {
		l.pos += len(str)
		l.emit(t)
		return lexText
	}
}

func lexIdentifier(str string, t itemType) stateFn {
	return func(l *lexer) stateFn {
		l.pos += len(str)
		l.ignore()
		l.acceptIdentifier()
		l.emit(t)

		return lexText
	}
}

func lexMatch(l *lexer) stateFn {
	l.pos += len("/")
	l.ignore()

	for {
		if strings.HasPrefix(l.input[l.pos:], "/") {
			l.emit(itemMatch)
			l.pos += len("/")
			l.ignore()

			return lexText
		}

		if l.next() == eof {
			return l.errorf("No closing / for match")
		}
	}

	panic("Unreachable")
}

func lexFunction(l *lexer) stateFn {
	l.emit(itemFunctionName)
	l.pos += len("(")
	l.ignore()

	for {
		if strings.HasPrefix(l.input[l.pos:], ")") {
			l.emit(itemFunctionParam)
			l.pos += len(")")
			l.ignore()

			return lexText
		}

		if l.next() == eof {
			break
		}
	}

	return lexText
}

// -------------------
// types and constants
// -------------------

// itemType identifies the type of lex items.
type itemType int

// item represents a token returned from the scanner.
type item struct {
	typ itemType // Type, such as itemNumber.
	val string   // Value, such as "23.2".
}

// lexer holds the state of the scanner.
type lexer struct {
	name       string    // used only for error reports.
	input      string    // the string being scanned.
	start      int       // start position of this item.
	pos        int       // current position in the input.
	width      int       // width of last rune read from input.
	items      chan item // channel of scanned items.
	commaStack int
	inSubexpr  bool
}

// The different types of items that can be returned
const (
	itemError itemType = iota
	itemText
	itemCluster
	itemClusterKey
	itemIntersect
	itemComma
	itemLeftGroup
	itemRightGroup
	itemFunctionName
	itemFunctionParam
	itemParam
	itemLocalClusterKey
	itemExclude
	itemGroupLookup
	itemSubexprStart
	itemSubexprEnd
	itemMatch
	itemEOF
)

const eof rune = -1

type stateFn func(*lexer) stateFn

// Pretty-print for items
func (i item) String() string {
	switch i.typ {
	case itemEOF:
		return "EOF"
	case itemCluster:
		return i.val
	case itemError:
		return i.val
	case itemRightGroup:
		return ",}"
	case itemSubexprStart:
		return "%{"
	case itemSubexprEnd:
		return "}%"
	case itemMatch:
		return fmt.Sprintf("/%s/", i.val)
	}
	if len(i.val) > 10 {
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// ------------------------
// generic helper functions
// ------------------------

func (l *lexer) emitTextIfPresent() {
	if l.pos > l.start {
		l.emit(itemText)
	}
}

// emit passes an item back to the client.
func (l *lexer) emit(t itemType) {
	x := item{t, l.input[l.start:l.pos]}
	// fmt.Printf("%s\n", x)
	l.items <- x
	l.start = l.pos
}

// next returns the next rune in the input.
func (l *lexer) next() (r rune) {
	if l.pos >= len(l.input) {
		l.width = 0
		return eof
	}
	r, l.width = utf8.DecodeRuneInString(l.input[l.pos:])
	l.pos += l.width
	return r
}

// ignore skips over the pending input before this point.
func (l *lexer) ignore() {
	l.start = l.pos
}

// acceptIdentifier consumes a run of runes that are allowed to be in an
// identifier.
func (l *lexer) acceptIdentifier() {
	for isAlphaNumeric(l.next()) {
	}
	l.backup()
}

// isAlphaNumeric reports whether r is an alphabetic, digit, hyphen, or
// underscore.
func isAlphaNumeric(r rune) bool {
	return r == '-' || r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

// error returns an error token and terminates the scan
// by passing back a nil pointer that will be the next
// state, terminating l.run.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.items <- item{
		itemError,
		fmt.Sprintf(format, args...),
	}
	return nil
}

// backup steps back one rune.
// Can be called only once per call of next.
func (l *lexer) backup() {
	l.pos -= l.width
}
