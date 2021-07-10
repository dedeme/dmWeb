// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// 'Ranges +' page.
package rangesPlus

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
	case "idata":
		modelId := cgi.RqString(mrq, "modelId")
		md, ok := fmodels.GetModel(modelId)
		if !ok {
			panic(modelId + " not found")
		}
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			var rankings []json.T
			for _, e := range fmodelsDb.ReadRangesPlus(lk, modelId) {
				rankings = append(rankings, e.ToJs())
			}
			rp["rankings"] = json.Wa(rankings)
			rp["parName"] = json.Ws(md.ParNames()[0])
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
