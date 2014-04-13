package grange

import (
	"reflect"
	"testing"
)

func TestDefaultCluster(t *testing.T) {
	testEval(t, []string{"b", "c"}, "%a", singleCluster("a", cluster{
		"CLUSTER": []string{"b", "c"},
	}))
}

func TestExplicitCluster(t *testing.T) {
	testEval(t, []string{"b", "c"}, "%a:NODES", singleCluster("a", cluster{
		"NODES": []string{"b", "c"},
	}))
}

func testEval(t *testing.T, expected []string, query string, state *rangeState) {
	actual := evalRange(query, state)

	if !reflect.DeepEqual(actual, expected) {
		t.Errorf("evalRange\n got: %v\nwant: %v", actual, expected)
	}
}

func singleCluster(name string, c cluster) *rangeState {
	state := rangeState{
		clusters: map[string]cluster{},
	}
	state.clusters[name] = c
	return &state
}
