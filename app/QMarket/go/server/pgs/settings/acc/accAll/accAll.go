// Copyright 07-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - Accounting-All page.
package accAll

import (
	"fmt"
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
	case "anns":
		year := cgi.RqString(mrq, "year")
		rp := map[string]json.T{}
		lock.Run(func() {
			rp["anns"] = diariesDb.ReadAllJs(year)
			cash, serrors := diariesDb.CashAll(year)
			if len(serrors) > 0 {
				logTb.Error(strings.Join(serrors, "\n"))
			}
			rp["cash"] = json.Wd(cash)
		})
		return cgi.Rp(ck, rp)
	case "cashUpTo":
		date := cgi.RqString(mrq, "date")
		rp := map[string]json.T{}
		lock.Run(func() {
			cash, serrors := diariesDb.CashAllUpTo(date)
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
