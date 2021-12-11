// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Models charts page.
package charts

import (
	"fmt"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/data/qtable"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "assets":
		qlevel := cgi.RqInt(mrq, "qlevel")
		id := cgi.RqInt(mrq, "id")
		rp := map[string]json.T{}
		lock.Run(func() {
			md := model.New(qlevel, id)
			opens := quotesDb.Opens()
			closes := quotesDb.Closes()

			var dates []json.T
			for _, e := range quotesDb.Dates() {
				dates = append(dates, json.Ws(e))
			}
			rp["dates"] = json.Wa(dates)

			var assets []json.T
			for _, e := range md.HistoricAssets(opens, closes) {
				assets = append(assets, json.Wd(e))
			}
			rp["assets"] = json.Wa(assets)
			rp["ok"] = json.Wb(true)
		})
		return cgi.Rp(ck, rp)
	case "ordersData":
		qlevel := cgi.RqInt(mrq, "qlevel")
		id := cgi.RqInt(mrq, "id")
		rp := map[string]json.T{}
		lock.Run(func() {
			dates := quotesDb.Dates()
			opens := quotesDb.Opens()
			closes := quotesDb.Closes()

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

			md := model.New(qlevel, id)
			var orders []json.T
			for _, e := range md.Orders(dates, opens, closes) {
				orders = append(orders, e.ToJs())
			}
			rp["orders"] = json.Wa(orders)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
