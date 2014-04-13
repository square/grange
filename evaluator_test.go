package grange

import (
	"reflect"
	"testing"
)

func TestEval(t *testing.T) {
	state := &rangeState{
		clusters: map[string]cluster{
			"a": cluster{
				"CLUSTER": []string{"b", "c"},
			},
		},
	}
	expected := []string{"b", "c"}
	actual := evalRange("%a", state)

	if !reflect.DeepEqual(actual, expected) {
		t.Errorf("evalRange\n got: %v\nwant: %v", actual, expected)
	}
}
