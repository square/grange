package grange

type cluster map[string][]string

type rangeState struct {
	clusters map[string]cluster
}

func evalRange(input string, state *rangeState) []string {
	_, items := lexRange("eval", input)

	fn := parseRange(items)

	return fn(state)
}

type evalFn func(*rangeState) []string

func parseRange(items chan item) evalFn {
	item := <-items

	clusterKey := "CLUSTER" // Default

	if item.typ == itemCluster {
		item = <-items

		if item.typ == itemText {
			clusterName := item.val

			return func(state *rangeState) []string {
				return clusterLookup(state, clusterName, clusterKey)
			}
		} else {
			panic("unimplemented")
		}
	} else {
		panic("unimplemented")
	}
}

func clusterLookup(state *rangeState, clusterName string, key string) []string {
	return state.clusters[clusterName][key] // TODO: Error handling
}
