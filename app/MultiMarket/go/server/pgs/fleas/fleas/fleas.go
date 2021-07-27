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
	case "best":
		modelId := cgi.RqString(mrq, "modelId")
		fmodel, ok := fmodels.GetModel(modelId)
		if !ok {
			panic("Model " + modelId + " not found")
		}
		rp := map[string]json.T{}
		rp["parName"] = json.Ws(fmodel.ParName())
		sync.Run(func(lk sync.T) {
			rp["best"] = json.Wn()
			ranks := fmodelsDb.Read(lk, modelId)
			if len(ranks) > 0 {
				efleas := ranks[0].Ranking()
				if len(efleas) > 0 {
					rp["best"] = efleas[0].ToJs()
				}
			}
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
