// Copyright 21-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// daily/daily page
package daily

import (
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/net"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":

		var act string
		var sv string
		var chartsData string
		thread.Sync(func() {
			act = db.ConfTb().Read().Activity.Activity
			svs := db.ServerBoxTb().Read().Servers
			if len(svs) == 0 {
				db.NextServer()
				svs = db.ServerBoxTb().Read().Servers
			}
			sv = svs[0]
			chartsData = db.DailyChartTb().ReadJs()
		})

		return cgi.Rp(ck, cgi.T{
			"activity":   js.Ws(act),
			"server":     js.Ws(sv),
			"chartsData": chartsData,
		})

	case "newServer":
		thread.Sync(func() {
			db.NextServer()
		})
		return cgi.RpEmpty(ck)

	case "reactivate":
		thread.Sync(func() {
			db.ActivateDailyCharts()
			db.UpdateDailyCharts(net.ServerReadDaily)
			db.UpdateHistoricProfits()
		})
		return cgi.RpEmpty(ck)

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
