// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting-balance page.
package balance

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/acc"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/db/acc/diariesDb"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/db/dailyTb"
	"github.com/dedeme/MultiMarket/db/investorsTb"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/db/refsDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			activity := conf.Activity(lk).Activity()
			dates := quotesDb.Dates(lk)
			dates2 := append(dates, date.Now().String())[1:]
			closes := quotesDb.Closes(lk)
			invs := investorsTb.Read(lk)
			qs := dailyTb.Read(lk)
			mqs := map[string]float64{}
			for _, q := range qs {
				nk, ok := nicksTb.GetNick(lk, q.Nick)
				if ok {
					mqs[nk.Name()] = q.Value
				}
			}

			var ledgers []json.T
			var portfolios []json.T
			for i, inv := range invs {
				anns := diariesDb.ReadAnnotations(lk, i)
				ledger, portfolio, _ := acc.Settlement(anns)
				ledgers = append(ledgers, ledger.ToJs())
				var pf []json.T
				for _, e := range portfolio {
					nk := e.Nick()
					quote := -1.0
					ref := e.Price()
					var dts = dates
					var cs [][]float64
					ok := false
					if activity != cts.ActSleeping2 {
						v, ok2 := mqs[nk]
						if ok2 {
							cs, ok = closes.NickValuesAdd(nk, v)
							dts = dates2
						} else {
							cs, ok = closes.NickValues(nk)
						}
					} else {
						cs, ok = closes.NickValues(nk)
					}
					if ok {
						mdPars := inv.GetModel(nk)
						quote = cs[len(cs)-1][0]
						refs := refsDb.MkRefs(
							lk, nk, dts, cs, mdPars.Model(), mdPars.Param(),
						)
						ref = refs[len(refs)-1]
						if ref > quote { // Sell situation.
							ref = quote
						}
					}
					pf = append(
						pf, acc.NewPfEntry(nk, e.Stocks(), e.Price(), quote, ref).ToJs(),
					)
				}
				portfolios = append(portfolios, json.Wa(pf))
			}

			rp["ledgers"] = json.Wa(ledgers)
			rp["portfolios"] = json.Wa(portfolios)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
