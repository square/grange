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

func testError(t *testing.T, input string, expected string) {
	testLex(t, input, item{itemError, expected})
}

func TestEmpty(t *testing.T) { testValid(t, "") }
func TestText(t *testing.T)  { testValid(t, "a.b", item{itemText, "a.b"}) }
func TestCluster(t *testing.T) {
	testValid(t, "%a", item{itemCluster, "%"}, item{itemText, "a"})
}
func TestPreamble(t *testing.T) { testError(t, "a%", "preceeding chars: a") }
func TestClusterKey(t *testing.T) {
	testValid(t, "%a:KEY",
		item{itemCluster, "%"},
		item{itemText, "a"},
		item{itemClusterKey, ":"},
		item{itemText, "KEY"},
	)
}
