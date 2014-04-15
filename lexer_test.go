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
	testValid(t, "%a", item{itemCluster, "a"})
}
func TestPreamble(t *testing.T) { testLexError(t, "a%", "preceeding chars: a") }
func TestClusterKey(t *testing.T) {
	testValid(t, "%a:KEY",
		item{itemCluster, "a"},
		item{itemClusterKey, "KEY"},
	)
}
func TestLexIntersect(t *testing.T) {
	testValid(t, "a & b",
		item{itemText, "a"},
		item{itemIntersect, "&"},
		item{itemText, "b"},
	)
}

func TestLexIntersectWithGroup(t *testing.T) {
	testValid(t, "a & %b:KEY",
		item{itemText, "a"},
		item{itemIntersect, "&"},
		item{itemCluster, "b"},
		item{itemClusterKey, "KEY"},
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

func TestFunction(t *testing.T) {
	testValid(t, "has(TYPE;db)",
		item{itemFunctionName, "has"},
		item{itemFunctionParam, "TYPE;db"},
	)
}

func TestLocalClusterKey(t *testing.T) {
	testValid(t, "$ALL",
		item{itemLocalClusterKey, "ALL"},
	)
}

func TestLexExclude(t *testing.T) {
	testValid(t, "$ALL - $DOWN",
		item{itemLocalClusterKey, "ALL"},
		item{itemExclude, "-"},
		item{itemLocalClusterKey, "DOWN"},
	)
}

func TestGroupLookup(t *testing.T) {
	testValid(t, "@dc",
		item{itemGroupLookup, "dc"},
	)
}
