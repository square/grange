package grange

import (
	"github.com/deckarep/golang-set"
	"reflect"
	"testing"
)

func TestDefaultCluster(t *testing.T) {
	testEval(t, NewResult("b", "c"), "%a", singleCluster("a", Cluster{
		"CLUSTER": []string{"b", "c"},
	}))
}

func TestExplicitCluster(t *testing.T) {
	testEval(t, NewResult("b", "c"), "%a:NODES", singleCluster("a", Cluster{
		"NODES": []string{"b", "c"},
	}))
}

func TestClusterKeys(t *testing.T) {
	testEval(t, NewResult("NODES"), "%a:KEYS", singleCluster("a", Cluster{
		"NODES": []string{"b", "c"},
	}))
}

func TestClusterMissing(t *testing.T) {
	testEval(t, NewResult(), "%a", emptyState())
}

func TestClusterMissingKey(t *testing.T) {
	testEval(t, NewResult(), "%a:NODES", singleCluster("a", Cluster{}))
}

func TestErrorExplicitCluster(t *testing.T) {
	testError(t, "Invalid token in query: \"}\"", "%a:}")
}

func TestErrorClusterName(t *testing.T) {
	testError(t, "Invalid token in query: \"}\"", "%}")
}

func TestHas(t *testing.T) {
	testEval(t, NewResult("a", "b"), "has(TYPE;one)", multiCluster(map[string]Cluster{
		"a": Cluster{"TYPE": []string{"one", "two"}},
		"b": Cluster{"TYPE": []string{"two", "one"}},
		"c": Cluster{"TYPE": []string{"three"}},
	}))
}

func TestHasIntersect(t *testing.T) {
	testEval(t, NewResult("b"), "has(TYPE;one)&b", multiCluster(map[string]Cluster{
		"a": Cluster{"TYPE": []string{"one", "two"}},
		"b": Cluster{"TYPE": []string{"two", "one"}},
		"c": Cluster{"TYPE": []string{"three"}},
	}))

	testEval(t, NewResult("b"), "has(TYPE;two)&has(TYPE;three)", multiCluster(map[string]Cluster{
		"a": Cluster{"TYPE": []string{"one", "two"}},
		"b": Cluster{"TYPE": []string{"two", "one", "three"}},
		"c": Cluster{"TYPE": []string{"three"}},
	}))
}

func TestIntersectEasy(t *testing.T) {
	testEval(t, NewResult("a"), "a & a", emptyState())
	testEval(t, NewResult(), "a & b", emptyState())
}

func TestIntersectCluster(t *testing.T) {
	testEval(t, NewResult("c"), "%a:L&%a:R", singleCluster("a", Cluster{
		"L": []string{"b", "c"},
		"R": []string{"c", "d"},
	}))
}

/*
// TODO: Pending
func TestIntersectError(t *testing.T) {
	testError(t, "No left side provided for intersection", "&a")
}
*/

func TestUnionEasy(t *testing.T) {
	testEval(t, NewResult("a", "b"), "a,b", emptyState())
}

func TestBracesWithUnion(t *testing.T) {
	testEval(t, NewResult("a.c", "b.c"), "{a,b}.c", emptyState())
	testEval(t, NewResult("a.b", "a.c"), "a.{b,c}", emptyState())
	testEval(t, NewResult("a.b.d", "a.c.d"), "a.{b,c}.d", emptyState())
}

func TestClusterUnion(t *testing.T) {
	testEval(t, NewResult("c", "d"), "%a,%b", multiCluster(map[string]Cluster{
		"a": Cluster{"CLUSTER": []string{"c"}},
		"b": Cluster{"CLUSTER": []string{"d"}},
	}))
}

/*
// TODO: Pending
func TestNoExpandInClusterName(t *testing.T) {
	testError(t, "Invalid token in query: \"{\"", "%a-{b,c}")
}
*/

func TestSelfReferentialCluster(t *testing.T) {
	testEval(t, NewResult("b"), "%a", multiCluster(map[string]Cluster{
		"a": Cluster{"CLUSTER": []string{"$ALL"}, "ALL": []string{"b"}},
	}))
}

func TestSelfReferentialClusterExpression(t *testing.T) {
	testEval(t, NewResult("a", "c"), "%a", multiCluster(map[string]Cluster{
		"a": Cluster{
			"CLUSTER": []string{"$ALL - $DOWN"},
			"ALL":     []string{"a", "b", "c"},
			"DOWN":    []string{"b"},
		},
	}))
}

func TestGroups(t *testing.T) {
	testEval(t, NewResult("a", "b"), "@dc", singleGroup("dc", "a", "b"))
}

func TestGroupsExpand(t *testing.T) {
	testEval(t, NewResult("c"), "@a", multiGroup(Cluster{
		"a": []string{"$b"},
		"b": []string{"c"},
	}))
}

func TestClusterLookup(t *testing.T) {
	testEval(t, NewResult("a"), "%{has(TYPE;db)}", singleCluster("ignore", Cluster{
		"CLUSTER": []string{"a"},
		"TYPE":    []string{"db"},
	}))
}

func TestClusterLookupExplicitKey(t *testing.T) {
	testEval(t, NewResult("a"), "%{has(TYPE;db)}:NODES", singleCluster("ignore", Cluster{
		"NODES": []string{"a"},
		"TYPE":  []string{"db"},
	}))
}

func TestClusterLookupDedup(t *testing.T) {
	testEval(t, NewResult("one", "two"), "%{has(TYPE;one)}:TYPE", multiCluster(map[string]Cluster{
		"a": Cluster{"TYPE": []string{"one", "two"}},
		"b": Cluster{"TYPE": []string{"two", "one"}},
		"c": Cluster{"TYPE": []string{"three"}},
	}))
}

func TestMatchNoContext(t *testing.T) {
	testEval(t, NewResult("ab"), "/b/", singleGroup("b", "ab", "c"))
}

func TestMatchEasy(t *testing.T) {
	testEval(t, NewResult("ab", "ba", "abc"), "%cluster & /b/",
		singleCluster("cluster", Cluster{
			"CLUSTER": []string{"ab", "ba", "abc", "ccc"},
		}))
}

func TestMatchReverse(t *testing.T) {
	testEval(t, NewResult("ab", "ba", "abc"), "/b/ & @group",
		singleGroup("group", "ab", "ba", "abc", "ccc"))
}

func TestMatchWithSubtract(t *testing.T) {
	testEval(t, NewResult("ccc"), "%cluster - /b/",
		singleCluster("cluster", Cluster{
			"CLUSTER": []string{"ab", "ba", "abc", "ccc"},
		}))
}

func TestUnionSubtractLeftAssociative(t *testing.T) {
	testEval(t, NewResult("a", "b-a"), "a,b-a", emptyState())
	testEval(t, NewResult("b"), "a , b - a", emptyState())
}

func TestInvalidLex(t *testing.T) {
	testError(t, "No closing / for match", "/")
}

func TestClusters(t *testing.T) {
	testEval(t, NewResult("a", "b"), "clusters(one)", multiCluster(map[string]Cluster{
		"a": Cluster{"CLUSTER": []string{"two", "one"}},
		"b": Cluster{"CLUSTER": []string{"$ALL"}, "ALL": []string{"one"}},
		"c": Cluster{"CLUSTER": []string{"three"}},
	}))
}

func TestClustersEasy(t *testing.T) {
	testEval(t, NewResult("a"), "clusters(one)", multiCluster(map[string]Cluster{
		"a": Cluster{"CLUSTER": []string{"two", "one"}},
	}))
}

func TestQ(t *testing.T) {
	testEval(t, NewResult("(/"), "q((/)", emptyState())
	testEval(t, NewResult("http://foo/bar?yeah"), "q(http://foo/bar?yeah)", emptyState())
}

func TestQueryGroups(t *testing.T) {
	testEval(t, NewResult("one", "two"), "?a", multiGroup(Cluster{
		"one":   []string{"a"},
		"two":   []string{"$one"},
		"three": []string{"b"},
	}))
}

func TestNumericRange(t *testing.T) {
	testEval(t, NewResult("n01", "n02", "n03"), "n01..n03", emptyState())
	testEval(t, NewResult("n10", "n11"), "n10..1", emptyState())
	testEval(t, NewResult("n01", "n02", "n03"), "n01..n3", emptyState())
	testEval(t, NewResult("1", "2", "3"), "1..3", emptyState())
	testEval(t, NewResult("n1", "n2", "n3"), "n1..3", emptyState())
	testEval(t, NewResult("n1", "n2", "n3"), "n1..n3", emptyState())
	testEval(t, NewResult(), "n2..n1", emptyState())
	testEval(t, NewResult("n9", "n10", "n11"), "n9..11", emptyState())
	testEval(t, NewResult("n1", "n2", "n3"), "n1..n03", emptyState())
	testEval(t, NewResult("n1..3"), "q(n1..3)", emptyState())
	testEval(t, NewResult("n10", "n11"), "n10..11", emptyState())

	// Different from crange, but shouldn't be relying on this anyway
	testEval(t, NewResult("n1an3..4", "n2an3..4"), "n1..2an3..4", emptyState())
}

func BenchmarkClusters(b *testing.B) {
	// setup fake state
	state := NewState()

	AddCluster(&state, "cluster", Cluster{
		"CLUSTER": []string{"$ALL"},
		"ALL":     []string{"mynode"},
	})
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		evalRange("clusters(mynode)", &state)
	}
}

func BenchmarkHas(b *testing.B) {
	// setup fake state
	state := NewState()

	AddCluster(&state, "cluster", Cluster{
		"CLUSTER": []string{"mynode"},
		"TYPE":    []string{"redis"},
	})
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		evalRange("has(TYPE;redis)", &state)
	}
}

func testError(t *testing.T, expected string, query string) {
	_, err := evalRange(query, emptyState())

	if err == nil {
		t.Errorf("Expected error but none returned")
	} else if err.Error() != expected {
		// TODO: Get error messages back
		//t.Errorf("Different error returned.\n got: %s\nwant: %s", err.Error(), expected)
	}
}

func testEval(t *testing.T, expected mapset.Set, query string, state *RangeState) {
	actual, err := evalRange(query, state)

	if err != nil {
		t.Errorf("Expected result, got error: %s", err)
	} else if !reflect.DeepEqual(actual, expected) {
		t.Errorf("evalRange\n got: %v\nwant: %v", actual, expected)
	}
}

func singleCluster(name string, c Cluster) *RangeState {
	state := NewState()
	state.clusters[name] = c
	return &state
}

func singleGroup(name string, members ...string) *RangeState {
	state := NewState()
	state.groups[name] = members
	return &state
}

func multiGroup(c Cluster) *RangeState {
	state := NewState()
	state.groups = c
	return &state
}

func multiCluster(cs map[string]Cluster) *RangeState {
	state := NewState()
	state.clusters = cs
	return &state
}

func emptyState() *RangeState {
	state := NewState()
	return &state
}
