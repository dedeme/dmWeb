// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting-balance page.
package balance

import (
	"fmt"
	"github.com/dedeme/QMarket/data/acc"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/db/acc/diariesDb"
	"github.com/dedeme/QMarket/db/confTb"
	"github.com/dedeme/QMarket/db/dailyDb/dailyTb"
	"github.com/dedeme/QMarket/db/investorsTb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/quotesDb"
	//"github.com/dedeme/QMarket/db/refsDb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		lock.Run(func() {
			activity := confTb.Activity().Activity()
			closes := quotesDb.Closes()
			invsTb := investorsTb.Read()
			qs := dailyTb.Read()
			mqs := map[string]float64{}
			for _, q := range qs {
				nk, ok := nicksTb.Read().NickFromId(q.Nick())
				if ok {
					mqs[nk.Name()] = q.Value()
				}
			}

			var ledgers []json.T
			var portfolios []json.T
			for i := 0; i < cts.Qlevels; i++ {
				anns := diariesDb.ReadAnnotations(i)
				ledger, portfolio, _ := acc.Settlement(anns)
				ledgers = append(ledgers, ledger.ToJs())
				var pf []json.T
				for _, e := range portfolio {
					nk := e.Nick()
					quote := -1.0
					ref := e.Price()
					var cs [][]float64
					ok := false
					if activity != cts.ActSleeping2 {
						v, ok2 := mqs[nk]
						if ok2 {
							cs, ok = closes.NickValuesAdd(nk, v)
						} else {
							cs, ok = closes.NickValues(nk)
						}
					} else {
						cs, ok = closes.NickValues(nk)
					}

					if ok {
						quote = cs[len(cs)-1][0]
						param := invsTb.Params[nk][i]
						refs := model.New(i, param).Refs(cs)
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
