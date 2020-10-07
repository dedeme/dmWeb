// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Daily charts page.
package daily

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/db/dailyChartsTb"
	"github.com/dedeme/MultiMarket/db/sboxTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			rp["chartsData"] = dailyChartsTb.ReadJs(lk)
			rp["server"] = json.Ws(sboxTb.GetServer(lk).Name())
			rp["activity"] = json.Ws(conf.Activity(lk).Activity())
		})
		return cgi.Rp(ck, rp)
	case "newServer":
		sync.Run(func(lk sync.T) {
			sboxTb.NextServer(lk)
		})
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
