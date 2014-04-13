package grange

import (
	"fmt"
	"strings"
	"unicode/utf8"
)

// A lexer of the form described in https://www.youtube.com/watch?v=HxaD_trXwRE
// State is represented with a function pointer, items are emitted on a
// channel.
func lexRange(name, input string) (*lexer, chan item) {
	l := &lexer{
		name:  name,
		input: input,
		items: make(chan item),
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
	for {
		if strings.HasPrefix(l.input[l.pos:], clusterMeta) {
			return lexCluster
		}
		if l.next() == eof {
			break
		}
	}
	if l.pos > l.start {
		l.emit(itemText)
	}
	l.emit(itemEOF)
	return nil
}

// A cluster reference is text prefixed by a constant identifier
const clusterMeta = "%"

func lexCluster(l *lexer) stateFn {
	l.pos += len(clusterMeta)
	l.ignore()
	for {
		if l.next() == eof {
			break
		}
	}
	if l.pos > l.start {
		l.emit(itemCluster)
	} else {
		return l.errorf("no cluster name")
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
	name  string    // used only for error reports.
	input string    // the string being scanned.
	start int       // start position of this item.
	pos   int       // current position in the input.
	width int       // width of last rune read from input.
	items chan item // channel of scanned items.
}

// The different types of items that can be returned
const (
	itemError itemType = iota
	itemText
	itemCluster
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
		return "%" + i.val
	case itemError:
		return i.val
	}
	if len(i.val) > 10 {
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// ------------------------
// generic helper functions
// ------------------------

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
