// Copyright 13-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// models orders test and charts page.
package orders

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
	case "ordersData":
		qlevel := cgi.RqInt(mrq, "qlevel")
		paramId := cgi.RqInt(mrq, "paramId")
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

			md := model.New(qlevel, paramId)
			md.FirstEvaluation(opens, closes)
      _, ev := md.Evaluation()
			rp["eval"] = ev.ToJs()

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
