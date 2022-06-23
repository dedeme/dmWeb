// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting settings page.
package acc

import (
	"github.com/dedeme/KtMarket/cts"
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
	case "idata":

		var yearsJs []string
		var annsJs string
		var cash float64

		thread.Sync(func() {
			years := diariesDb.Years()
			yearsJs = arr.Map(years, js.Ws)
			lastYear := years[0]
			annsJs = diariesDb.ReadAllJs(lastYear)
			var serrors []string
			cash, serrors = diariesDb.CashAll(lastYear)
			if len(serrors) > 0 {
				log.Error(arr.Join(serrors, "\n"))
			}
		})

		return cgi.Rp(ck, cgi.T{
			"investors": js.Wi(cts.Investors),
			"years":     js.Wa(yearsJs),
			"anns":      annsJs,
			"cash":      js.Wd(cash),
		})
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
