// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Daily charts page.
package daily

import (
	"fmt"
	"github.com/dedeme/QMarket/db/confTb"
	"github.com/dedeme/QMarket/db/dailyDb/dailyChartsTb"
	"github.com/dedeme/QMarket/db/dailyDb/sboxTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/QMarket/scheduler"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		lock.Run(func() {
			rp["chartsData"] = dailyChartsTb.ReadJs()
			rp["server"] = json.Ws(sboxTb.GetServer().Name())
			rp["activity"] = json.Ws(confTb.Activity().Activity())
		})
		return cgi.Rp(ck, rp)
	case "newServer":
		lock.Run(func() {
			sboxTb.NextServer()
		})
		return cgi.RpEmpty(ck)
	case "reactivate":
		lock.Run(func() { scheduler.Reactivate() })
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
