// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// 'fleas/ranking' page.
package rankings

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
			for _, e := range fmodelsDb.Read(lk, modelId) {
				rankings = append(rankings, e.ToJs())
			}
			rp["rankings"] = json.Wa(rankings)
			rp["parName"] = json.Ws(md.ParName())
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
