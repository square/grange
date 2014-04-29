package grange

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestExpand(t *testing.T) {
	spec_dir := os.Getenv("RANGE_SPEC_PATH")
	if spec_dir == "" {
		// Skip compress tests
		fmt.Fprintln(os.Stderr, "Skipping Expand() tests, RANGE_SPEC_PATH not set.")
		return
	}

	filepath.Walk(spec_dir+"/spec/expand", func(path string, info os.FileInfo, err error) error {
		if !info.IsDir() {
			return nil
		}

		specs, err := filepath.Glob(path + "/*.spec")
		if err == nil && specs != nil {
			for _, spec := range specs {
				loadExpandSpec(t, spec)
			}
		}
		return nil
	})
}

func runExpandSpec(t *testing.T, spec RangeSpec) {
	state := NewState()
	actual, _ := state.Query(spec.expr)

	if !reflect.DeepEqual(actual, spec.results) {
		t.Errorf("failed %s:%d\n got: %s\nwant: %s",
			spec.path, spec.line, actual, spec.results)
	}
}

func loadExpandSpec(t *testing.T, specpath string) {
	file, _ := os.Open(specpath)
	scanner := bufio.NewScanner(file)
	currentSpec := RangeSpec{results: NewResult(), path: specpath}

	line := 0
	for scanner.Scan() {
		line++
		if strings.HasPrefix(strings.Trim(scanner.Text(), " "), "#") {
			continue
		} else if scanner.Text() == "" {
			runExpandSpec(t, currentSpec)
			currentSpec = RangeSpec{results: NewResult(), path: specpath}
		} else {
			if currentSpec.expr == "" {
				currentSpec.expr = scanner.Text()
				currentSpec.line = line
			} else {
				currentSpec.results.Add(scanner.Text())
			}
		}
	}
	if currentSpec.expr != "" {
		runExpandSpec(t, currentSpec)
	}
}
