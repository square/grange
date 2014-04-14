package grange

import (
	"reflect"
	"testing"
)

func TestDefaultCluster(t *testing.T) {
	testEval(t, []string{"b", "c"}, "%a", singleCluster("a", Cluster{
		"CLUSTER": []string{"b", "c"},
	}))
}

func TestExplicitCluster(t *testing.T) {
	testEval(t, []string{"b", "c"}, "%a:NODES", singleCluster("a", Cluster{
		"NODES": []string{"b", "c"},
	}))
}

func TestErrorExplicitCluster(t *testing.T) {
	testError(t, "Invalid token in query: \"}\"", "%a:}")
}

func TestErrorClusterName(t *testing.T) {
	testError(t, "Invalid token in query: \"}\"", "%}")
}

func TestHas(t *testing.T) {
	testEval(t, []string{"a", "b"}, "has(TYPE;one)", multiCluster(map[string]Cluster{
		"a": Cluster{"TYPE": []string{"one", "two"}},
		"b": Cluster{"TYPE": []string{"two", "one"}},
		"c": Cluster{"TYPE": []string{"three"}},
	}))
}

func TestHasIntersect(t *testing.T) {
	testEval(t, []string{"b"}, "has(TYPE;one)&b", multiCluster(map[string]Cluster{
		"a": Cluster{"TYPE": []string{"one", "two"}},
		"b": Cluster{"TYPE": []string{"two", "one"}},
		"c": Cluster{"TYPE": []string{"three"}},
	}))

	testEval(t, []string{"b"}, "has(TYPE;two)&has(TYPE;three)", multiCluster(map[string]Cluster{
		"a": Cluster{"TYPE": []string{"one", "two"}},
		"b": Cluster{"TYPE": []string{"two", "one", "three"}},
		"c": Cluster{"TYPE": []string{"three"}},
	}))
}

func TestIntersect(t *testing.T) {
	testEval(t, []string{"c"}, "%a:L&%a:R", singleCluster("a", Cluster{
		"L": []string{"b", "c"},
		"R": []string{"c", "d"},
	}))
}

func TestIntersectError(t *testing.T) {
	testError(t, "No left side provided for intersection", "&a")
}

func TestExpand(t *testing.T) {
	testEval(t, []string{"a", "b"}, "a,b", emptyState())
}

func TestGroupExpand(t *testing.T) {
	testEval(t, []string{"a.c", "b.c"}, "{a,b}.c", emptyState())
	testEval(t, []string{"a.b", "a.c"}, "a.{b,c}", emptyState())
}

func TestClusterExpand(t *testing.T) {
	testEval(t, []string{"c", "d"}, "%a,%b", multiCluster(map[string]Cluster{
		"a": Cluster{"CLUSTER": []string{"c"}},
		"b": Cluster{"CLUSTER": []string{"d"}},
	}))
}

func TestNoExpandInClusterName(t *testing.T) {
	testError(t, "Invalid token in query: \"{\"", "%a-{b,c}")
}

func testError(t *testing.T, expected string, query string) {
	_, err := evalRange(query, emptyState())

	if err == nil {
		t.Errorf("Expected error but none returned")
	} else if err.Error() != expected {
		t.Errorf("Different error returned.\n got: %s\nwant: %s", err.Error(), expected)
	}
}

func testEval(t *testing.T, expected []string, query string, state *RangeState) {
	actual, err := evalRange(query, state)

	if err != nil {
		t.Errorf("Expected result, got error: %s", err)
	} else if !reflect.DeepEqual(actual, expected) {
		t.Errorf("evalRange\n got: %v\nwant: %v", actual, expected)
	}
}

func singleCluster(name string, c Cluster) *RangeState {
	state := RangeState{
		clusters: map[string]Cluster{},
	}
	state.clusters[name] = c
	return &state
}

func multiCluster(cs map[string]Cluster) *RangeState {
	state := RangeState{
		clusters: cs,
	}
	return &state
}

func emptyState() *RangeState {
	return &RangeState{}
}
