// Copyright 07-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - Accounting-Editor page.
package accEditor

import (
	"fmt"
	"github.com/dedeme/QMarket/data/acc"
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
		year := cgi.RqString(mrq, "year")
		investorId := cgi.RqInt(mrq, "investorId")
		rp := map[string]json.T{}
		lock.Run(func() {
			anns, ok := diariesDb.ReadJs(investorId, year)
			if ok {
				rp["anns"] = anns
				var as []*acc.AnnotationT
				for _, e := range anns.Ra() {
					as = append(as, acc.AnnotationFromJs(e))
				}
				ledger, _, serrors := acc.Settlement(as)
				if len(serrors) > 0 {
					logTb.Error(strings.Join(serrors, "\n"))
				}
				rp["cash"] = json.Wd(ledger.Cash())
			} else {
				rp["anns"] = json.Wa([]json.T{})
				rp["cash"] = json.Wd(0.0)
			}
		})
		return cgi.Rp(ck, rp)
	case "new":
		investorId := cgi.RqInt(mrq, "investorId")
		ann := acc.AnnotationFromJs(mrq["ann"])
		rp := map[string]json.T{}
		lock.Run(func() {
			year := diariesDb.Years()[0]
			diariesDb.Add(investorId, year, ann)
			anns, ok := diariesDb.ReadJs(investorId, year)
			if ok {
				rp["anns"] = anns
				var as []*acc.AnnotationT
				for _, e := range anns.Ra() {
					as = append(as, acc.AnnotationFromJs(e))
				}
				ledger, _, serrors := acc.Settlement(as)
				if len(serrors) > 0 {
					logTb.Error(strings.Join(serrors, "\n"))
				}
				rp["cash"] = json.Wd(ledger.Cash())
			} else {
				rp["anns"] = json.Wa([]json.T{})
				rp["cash"] = json.Wd(0.0)
			}
		})
		return cgi.Rp(ck, rp)
	case "del":
		investorId := cgi.RqInt(mrq, "investorId")
		annId := cgi.RqInt(mrq, "annId")
		rp := map[string]json.T{}
		lock.Run(func() {
			year := diariesDb.Years()[0]
			diariesDb.Del(investorId, year, annId)
			anns, ok := diariesDb.ReadJs(investorId, year)
			if ok {
				rp["anns"] = anns
				var as []*acc.AnnotationT
				for _, e := range anns.Ra() {
					as = append(as, acc.AnnotationFromJs(e))
				}
				ledger, _, serrors := acc.Settlement(as)
				if len(serrors) > 0 {
					logTb.Error(strings.Join(serrors, "\n"))
				}
				rp["cash"] = json.Wd(ledger.Cash())
			} else {
				rp["anns"] = json.Wa([]json.T{})
				rp["cash"] = json.Wd(0.0)
			}
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
