// Copyright 13-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas orders test page.
package ftestsOrders

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/qtable"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "ordersData":
		modelId := cgi.RqString(mrq, "modelId")
		var params []float64
		for _, e := range mrq["params"].Ra() {
			params = append(params, e.Rd())
		}
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			dates := quotesDb.Dates(lk)
			opens := quotesDb.Opens(lk)
			closes := quotesDb.Closes(lk)
			md, ok := fmodels.GetModel(modelId)
			if !ok {
				log.Error(lk, "Model "+modelId+" not found")
				rp["ok"] = json.Wb(false)
				return
			}

			var nicks []json.T
			for _, e := range opens.Nicks() {
				nicks = append(nicks, json.Ws(e))
			}
			rp["nicks"] = json.Wa(nicks)

			var lastCloses []json.T
			for i := 0; i < len(nicks); i++ {
				lastCloses = append(
					lastCloses, json.Wd(qtable.LastRowOk(closes.Values(), i)),
				)
			}
			rp["lastCloses"] = json.Wa(lastCloses)

			rp["assets"] = md.Assets(opens, closes, params).ToJs()

			var orders []json.T
			for _, e := range md.Orders(dates, opens, closes, params) {
				orders = append(orders, e.ToJs())
			}
			rp["orders"] = json.Wa(orders)

			rp["ok"] = json.Wb(true)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
