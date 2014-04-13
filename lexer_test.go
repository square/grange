package grange

import (
	"reflect"
	"testing"
)

func testLex(t *testing.T, input string, expected ...item) {
	_, items := lexRange("test", input)

	actual := make([]item, 0)

	for msg := range items {
		actual = append(actual, msg)
	}

	if !reflect.DeepEqual(actual, expected) {
		t.Errorf("lexRange(%v) =\n got: %v\nwant: %v", input, actual, expected)
	}
}

func testValid(t *testing.T, input string, expected ...item) {
	// Every valid lex is terminated with EOF
	testLex(t, input, append(expected, item{itemEOF, ""})...)
}

func testLexError(t *testing.T, input string, expected string) {
	testLex(t, input, item{itemError, expected})
}

func TestEmpty(t *testing.T) { testValid(t, "") }
func TestText(t *testing.T)  { testValid(t, "a.b", item{itemText, "a.b"}) }
func TestCluster(t *testing.T) {
	testValid(t, "%a", item{itemCluster, "%"}, item{itemText, "a"})
}
func TestPreamble(t *testing.T) { testLexError(t, "a%", "preceeding chars: a") }
func TestClusterKey(t *testing.T) {
	testValid(t, "%a:KEY",
		item{itemCluster, "%"},
		item{itemText, "a"},
		item{itemClusterKey, ":"},
		item{itemText, "KEY"},
	)
}
func TestLexIntersect(t *testing.T) {
	testValid(t, "a&b",
		item{itemText, "a"},
		item{itemIntersect, "&"},
		item{itemText, "b"},
	)
}

func TestLexIntersectWithGroup(t *testing.T) {
	testValid(t, "a&%b:KEY",
		item{itemText, "a"},
		item{itemIntersect, "&"},
		item{itemCluster, "%"},
		item{itemText, "b"},
		item{itemClusterKey, ":"},
		item{itemText, "KEY"},
	)
}

func TestComma(t *testing.T) {
	testValid(t, "a,b,c",
		item{itemText, "a"},
		item{itemComma, ","},
		item{itemText, "b"},
		item{itemComma, ","},
		item{itemText, "c"},
	)
}

func TestGroup(t *testing.T) {
	testValid(t, "{a}",
		item{itemLeftGroup, "{"},
		item{itemText, "a"},
		item{itemRightGroup, "}"},
	)
}
