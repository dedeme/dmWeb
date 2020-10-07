// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas page.
package fleas

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/db/fleas/fmodelsDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "models":
		var models []json.T
		for _, e := range fmodels.List() {
			models = append(models, e.ToJs())
		}
		rp := map[string]json.T{}
		rp["models"] = json.Wa(models)
		return cgi.Rp(ck, rp)
	case "ranking":
		modelId := cgi.RqString(mrq, "modelId")
		fmodel, ok := fmodels.GetModel(modelId)
		if !ok {
			panic("Model " + modelId + " not found")
		}
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			var ranking []json.T
			for _, e := range fmodelsDb.ReadRanking(lk, modelId) {
				ranking = append(ranking, e.ToJs())
			}
			rp["ranking"] = json.Wa(ranking)

			var parNames []json.T
			for _, e := range fmodel.ParNames() {
				parNames = append(parNames, json.Ws(e))
			}
			rp["parNames"] = json.Wa(parNames)

			var parDecs []json.T
			for _, e := range fmodel.ParDecs() {
				parDecs = append(parDecs, json.Wi(e))
			}
			rp["parDecs"] = json.Wa(parDecs)
		})
		return cgi.Rp(ck, rp)
	case "bests":
		modelId := cgi.RqString(mrq, "modelId")
		fmodel, ok := fmodels.GetModel(modelId)
		if !ok {
			panic("Model " + modelId + " not found")
		}
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			var bests []json.T
			for _, e := range fmodelsDb.ReadBests(lk, modelId) {
				bests = append(bests, e.ToJs())
			}
			rp["bests"] = json.Wa(bests)

			var parNames []json.T
			for _, e := range fmodel.ParNames() {
				parNames = append(parNames, json.Ws(e))
			}
			rp["parNames"] = json.Wa(parNames)

			var parDecs []json.T
			for _, e := range fmodel.ParDecs() {
				parDecs = append(parDecs, json.Wi(e))
			}
			rp["parDecs"] = json.Wa(parDecs)
		})
		return cgi.Rp(ck, rp)
	case "pool":
		modelId := cgi.RqString(mrq, "modelId")
		fmodel, ok := fmodels.GetModel(modelId)
		if !ok {
			panic("Model " + modelId + " not found")
		}
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			var pool []json.T
			for _, e := range fmodelsDb.ReadPool(lk, modelId) {
				pool = append(pool, e.ToJs())
			}
			rp["pool"] = json.Wa(pool)

			var parNames []json.T
			for _, e := range fmodel.ParNames() {
				parNames = append(parNames, json.Ws(e))
			}
			rp["parNames"] = json.Wa(parNames)

			var parDecs []json.T
			for _, e := range fmodel.ParDecs() {
				parDecs = append(parDecs, json.Wi(e))
			}
			rp["parDecs"] = json.Wa(parDecs)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
