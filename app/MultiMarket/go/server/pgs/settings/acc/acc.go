// Copyright 03-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - Accounting page.
package acc

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/db/acc/diariesDb"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"strings"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			rp["investors"] = json.Wi(cts.Investors)

			years := diariesDb.Years(lk)
			var yearsJs []json.T
			for _, e := range years {
				yearsJs = append(yearsJs, json.Ws(e))
			}
			rp["years"] = json.Wa(yearsJs)

			lastYear := years[0]
			rp["anns"] = diariesDb.ReadAllJs(lk, lastYear)

			cash, serrors := diariesDb.CashAll(lk, lastYear)
			if len(serrors) > 0 {
				log.Error(lk, strings.Join(serrors, "\n"))
			}
			rp["cash"] = json.Wd(cash)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
