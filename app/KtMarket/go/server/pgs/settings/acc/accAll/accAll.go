// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - Accounting-All page.
package accAll

import (
	"github.com/dedeme/KtMarket/db/acc/diariesDb"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "anns":
		year := js.Rs(mrq["year"])

		var anns string
		var cash float64

		thread.Sync(func() {
			anns = diariesDb.ReadAllJs(year)
			var serrors []string
			cash, serrors = diariesDb.CashAll(year)
			if len(serrors) > 0 {
				log.Error(arr.Join(serrors, "\n"))
			}
		})

		return cgi.Rp(ck, cgi.T{
			"anns": anns,
			"cash": js.Wd(cash),
		})
	case "cashUpTo":
		date := js.Rs(mrq["date"])

		var cash float64

		thread.Sync(func() {
			var serrors []string
			cash, serrors = diariesDb.CashAllUpTo(date)
			if len(serrors) > 0 {
				log.Error(arr.Join(serrors, "\n"))
			}
		})

		return cgi.Rp(ck, cgi.T{
			"cash": js.Wd(cash),
		})
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
