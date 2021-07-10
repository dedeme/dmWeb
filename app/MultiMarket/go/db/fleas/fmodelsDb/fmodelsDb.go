// Copyright 11-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas models data base.
package fmodelsDb

import (
	"github.com/dedeme/MultiMarket/data/flea/eval"
	"github.com/dedeme/MultiMarket/data/flea/evalDate"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/flea/frank"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var parentDir string

func modelDir(modelId string) string {
	return path.Join(parentDir, modelId)
}

func poolPath(modelId string) string {
	return path.Join(modelDir(modelId), "Pool.tb")
}

func bestsPath(modelId string) string {
	return path.Join(modelDir(modelId), "Bests.tb")
}

func rankingPath(modelId string) string {
	return path.Join(modelDir(modelId), "Ranking.tb")
}

func rangesPlusPath(modelId string) string {
	return path.Join(modelDir(modelId), "RangesPlus.tb")
}

func Initialize(lk sync.T, parent string) {
	parentDir = parent
	for _, e := range fmodels.List() {
		id := e.Id()
		if !file.Exists(modelDir(id)) {
			file.Mkdir(modelDir(id))
			WritePool(lk, id, []*eval.T{})
			WriteBests(lk, id, []*evalDate.T{})
			WriteRanking(lk, id, []*frank.T{})
			if len(e.ParNames()) == 1 {
				WriteRangesPlus(lk, id, []*frank.T{})
			}
		}
	}
}

// Writes an evaluated fleas pool.
//   lk     : Synchronization lock.
//   modelId: Model identifier.
//   efleas : Evaluated fleas.
func WritePool(lk sync.T, modelId string, efleas []*eval.T) {
	var a []json.T
	for _, e := range efleas {
		a = append(a, e.ToJs())
	}
	file.WriteAll(poolPath(modelId), json.Wa(a).String())
}

// Reads an evaluated fleas pool.
//   lk     : Synchronization lock.
//   modelId: Model identifier.
func ReadPool(lk sync.T, modelId string) []*eval.T {
	var a []*eval.T
	for _, e := range json.FromString(file.ReadAll(poolPath(modelId))).Ra() {
		a = append(a, eval.FromJs(e))
	}
	return a
}

// Writes an evaluated fleas pool of bests ones.
//   lk     : Synchronization lock.
//   modelId: Model identifier.
//   efleas : Evaluated fleas.
func WriteBests(lk sync.T, modelId string, efleas []*evalDate.T) {
	var a []json.T
	for _, e := range efleas {
		a = append(a, e.ToJs())
	}
	file.WriteAll(bestsPath(modelId), json.Wa(a).String())
}

// Reads an evaluated fleas pool of bests ones.
//   lk     : Synchronization lock.
//   modelId: Model identifier.
func ReadBests(lk sync.T, modelId string) []*evalDate.T {
	var a []*evalDate.T
	for _, e := range json.FromString(file.ReadAll(bestsPath(modelId))).Ra() {
		a = append(a, evalDate.FromJs(e))
	}
	return a
}

// Writes an evaluated fleas ranking.
//   lk     : Synchronization lock.
//   modelId: Model identifier.
//   ranks  : Evaluated fleas rankings (sorted from after to before).
func WriteRanking(lk sync.T, modelId string, ranks []*frank.T) {
	var a []json.T
	for _, e := range ranks {
		a = append(a, e.ToJs())
	}
	file.WriteAll(rankingPath(modelId), json.Wa(a).String())
}

// Reads evaluated fleas rankings (sorted from after to before).
//   lk     : Synchronization lock.
//   modelId: Model identifier.
func ReadRanking(lk sync.T, modelId string) []*frank.T {
	var a []*frank.T
	for _, e := range json.FromString(file.ReadAll(rankingPath(modelId))).Ra() {
		a = append(a, frank.FromJs(e))
	}
	return a
}

// Writes an evaluated fleas rangesPlus ranking.
//   lk     : Synchronization lock.
//   modelId: Model identifier.
//   ranks  : Evaluated fleas rankings (sorted from after to before).
func WriteRangesPlus(lk sync.T, modelId string, ranks []*frank.T) {
	var a []json.T
	for _, e := range ranks {
		a = append(a, e.ToJs())
	}
	file.WriteAll(rangesPlusPath(modelId), json.Wa(a).String())
}

// Reads evaluated fleas rangesPlus rankings (sorted from after to before).
//   lk     : Synchronization lock.
//   modelId: Model identifier.
func ReadRangesPlus(lk sync.T, modelId string) []*frank.T {
	var a []*frank.T
	for _, e := range json.FromString(file.ReadAll(rangesPlusPath(modelId))).Ra() {
		a = append(a, frank.FromJs(e))
	}
	return a
}
