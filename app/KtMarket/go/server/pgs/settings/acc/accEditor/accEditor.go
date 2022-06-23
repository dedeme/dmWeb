// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - Accounting-Editor page.package accEditor
package accEditor

import (
	"github.com/dedeme/KtMarket/data/acc"
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

		year := js.Rs(mrq["year"])
		investorId := js.Ri(mrq["investorId"])

		var anns string
		var cash float64

		thread.Sync(func() {
			var ok bool
			anns, ok = diariesDb.ReadJs(investorId, year)
			if ok {
				as := arr.Map(js.Ra(anns), acc.AnnotationFromJs)
				var serrors []string
				ledger, _, _, serrors := acc.Settlement(as)
				if len(serrors) > 0 {
					log.Error(arr.Join(serrors, "\n"))
				}
				cash = ledger.Cash
			} else {
				anns = js.Wa([]string{})
				cash = 0.0
			}
		})

		return cgi.Rp(ck, cgi.T{
			"anns": anns,
			"cash": js.Wd(cash),
		})
	case "new":

		investorId := js.Ri(mrq["investorId"])
		ann := acc.AnnotationFromJs(mrq["ann"])

		var anns string
		var cash float64

		thread.Sync(func() {
			year := diariesDb.Years()[0]
			diariesDb.Add(investorId, year, ann)
			var ok bool
			anns, ok = diariesDb.ReadJs(investorId, year)
			if ok {
				as := arr.Map(js.Ra(anns), acc.AnnotationFromJs)
				var serrors []string
				ledger, _, _, serrors := acc.Settlement(as)
				if len(serrors) > 0 {
					log.Error(arr.Join(serrors, "\n"))
				}
				cash = ledger.Cash
			} else {
				anns = js.Wa([]string{})
				cash = 0.0
			}
		})

		return cgi.Rp(ck, cgi.T{
			"anns": anns,
			"cash": js.Wd(cash),
		})
	case "del":

		investorId := js.Ri(mrq["investorId"])
		annId := js.Ri(mrq["annId"])

		var anns string
		var cash float64

		thread.Sync(func() {
			year := diariesDb.Years()[0]
			diariesDb.Del(investorId, year, annId)
			var ok bool
			anns, ok = diariesDb.ReadJs(investorId, year)
			if ok {
				as := arr.Map(js.Ra(anns), acc.AnnotationFromJs)
				var serrors []string
				ledger, _, _, serrors := acc.Settlement(as)
				if len(serrors) > 0 {
					log.Error(arr.Join(serrors, "\n"))
				}
				cash = ledger.Cash
			} else {
				anns = js.Wa([]string{})
				cash = 0.0
			}
		})

		return cgi.Rp(ck, cgi.T{
			"anns": anns,
			"cash": js.Wd(cash),
		})
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
