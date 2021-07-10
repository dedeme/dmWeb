// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Parameters of fleas ranges table.
package rangesTb

import (
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initializes table..
//    parent: Parent directory of "Ranges.tb".
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Ranges.tb")
	if !file.Exists(fpath) {
		file.WriteAll(fpath, json.Wo(map[string]json.T{}).String())
	}
}

// Writes a model parameters.
//    lk     : Synchronization lock.
//    modelId: Model identifier.
//    param  : Range base with 'len' from 0 to 3 (both inclusive).
//             For example:
//                "[]" fix the closed range 5,0 to 35,9
//                "[7]" fix the closed range 7,00 to 7,99
//                "[14, 2]" fix the closed range 14,200 to 14,299.
//                "[14, 2, 4]" fix the closed range 14,2400 to 14,2499.
func Write(lk sync.T, modelId string, param []int) {
	data := Read(lk)
	data[modelId] = param

	dataJs := map[string]json.T{}
	for k, v := range data {
		var es []json.T
		for _, e := range v {
			es = append(es, json.Wi(e))
		}
		dataJs[k] = json.Wa(es)
	}

	file.WriteAll(fpath, json.Wo(dataJs).String())
}

// Reads parameters of every model (see Write()).
//    lk: Synchronization lock.
func Read(lk sync.T) map[string][]int {
	data := map[string][]int{}

	dataJs := json.FromString(file.ReadAll(fpath)).Ro()
	for k, v := range dataJs {
		var es []int
		for _, e := range v.Ra() {
			es = append(es, e.Ri())
		}
		data[k] = es
	}

	return data
}

// Reads parameters of a model.
//    lk: Synchronization lock.
//    modelId: Model identifier.
func ReadModel(lk sync.T, modelId string) []int {
	if param, ok := Read(lk)[modelId]; ok {
		return param
	}
	return []int{}
}

// Removes entries not contained in 'modelIds'.
//    lk      : Synchronization lock.
//    modelIds: List of models with only one parameter.
func Clean(lk sync.T, modelIds []string) {
	dataJs := json.FromString(file.ReadAll(fpath)).Ro()
	var toDel []string
	for k := range dataJs {
		missing := true
		for _, k2 := range modelIds {
			if k == k2 {
				missing = false
				break
			}
		}
		if missing {
			toDel = append(toDel, k)
		}
	}

	for _, k := range toDel {
		delete(dataJs, k)
	}

	file.WriteAll(fpath, json.Wo(dataJs).String())
}
