// Copyright 11-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas models data base.
package fmodelsDb

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/flea/frank"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var parentDir string

func modelPath(modelId string) string {
	return path.Join(parentDir, modelId+".tb")
}

func Initialize(lk sync.T, parent string) {
	parentDir = parent
	for _, e := range fmodels.List() {
		id := e.Id()
		if !file.Exists(modelPath(id)) {
			Write(lk, id, []*frank.T{})
		}
	}
}

// Writes a 'modelId' ranking.
//   lk     : Synchronization lock.
//   modelId: Model identifier.
//   ranks  : Evaluated fleas rankings (sorted from after to before).
func Write(lk sync.T, modelId string, ranks []*frank.T) {
	var a []json.T
	for i, e := range ranks {
		if i < cts.RankingDays {
			a = append(a, e.ToJs())
		}
	}
	file.WriteAll(modelPath(modelId), json.Wa(a).String())
}

// Reads a 'modelId' ranking.
//   lk     : Synchronization lock.
//   modelId: Model identifier.
func Read(lk sync.T, modelId string) []*frank.T {
	var a []*frank.T
	for _, e := range json.FromString(file.ReadAll(modelPath(modelId))).Ra() {
		a = append(a, frank.FromJs(e))
	}
	return a
}
