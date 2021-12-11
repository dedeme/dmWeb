// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Models page.
package models

import (
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/db/ranksDb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		rp["qlevels"] = json.Wi(cts.Qlevels)
		return cgi.Rp(ck, rp)
	case "best":
		qlevel := cgi.RqInt(mrq, "qlevel")
		rp := map[string]json.T{}
		lock.Run(func() {
			rp["best"] = json.Wn()
			ranks := ranksDb.Read(qlevel)
			if len(ranks) > 0 {
				rk := ranks[0].ModelRank()
				if len(rk) > 0 {
					rp["best"] = rk[0].ToJs()
				}
			}
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
