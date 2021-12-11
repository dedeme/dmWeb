// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - Accounting page.
package acc

import (
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/db/acc/diariesDb"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"strings"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		lock.Run(func() {
			rp["investors"] = json.Wi(cts.Qlevels)

			years := diariesDb.Years()
			var yearsJs []json.T
			for _, e := range years {
				yearsJs = append(yearsJs, json.Ws(e))
			}
			rp["years"] = json.Wa(yearsJs)

			lastYear := years[0]
			rp["anns"] = diariesDb.ReadAllJs(lastYear)

			cash, serrors := diariesDb.CashAll(lastYear)
			if len(serrors) > 0 {
				logTb.Error(strings.Join(serrors, "\n"))
			}
			rp["cash"] = json.Wd(cash)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
