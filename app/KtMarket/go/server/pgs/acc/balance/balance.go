// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// acc/balance page
package balance

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/acc"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/refBase"
	"github.com/dedeme/KtMarket/data/strategy"
	"github.com/dedeme/KtMarket/db"
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

		var ok bool
		var ledgers []string
		var portfolios []string

		thread.Sync(func() {
			refs := db.RefBasesTb().Read().Refs
			invs := db.InvestorsTb().Read().Investors
			for i := 0; i < cts.Investors; i++ {
				inv := invs[i]
				nks := db.NicksTb().Read().List
				anns := diariesDb.ReadAnnotations(i)
				ledger, portfolio, _, es := acc.Settlement(anns)
				if len(es) != 0 {
					for i := 0; i < len(es); i++ {
						log.Error(es[i])
					}
					return
				}

				var pf []*acc.PfEntryT
				for _, e := range portfolio {
					nk, ok := arr.Find(nks, func(nk *nick.T) bool {
						return nk.Name == e.Nick
					})
					if !ok {
						log.Error("Nick " + e.Nick + " not found")
						return
					}
					_, closes, err := db.CurrentCloses(nk)
					if err != "" {
						log.Error(err)
						return
					}

					st, ok2 := inv.Nicks[e.Nick]
					if !ok2 {
						log.Error("Strategy for " + e.Nick + " not found")
						return
					}

					ref := -1.0
					initRef, ok := arr.Find(refs, func(r *refBase.T) bool {
						return r.Nick.Name == e.Nick && strategy.Eq(r.Strategy, st)
					})
					if ok {
						ref = initRef.Ref
						closes = arr.Drop(closes, len(closes)-cts.ReferenceQuotes)
					}

					lastClose := closes[len(closes)-1]
					lastRef := strategy.LastRef(st, closes, ref)
					if lastRef > lastClose { // Sell situation.
						lastRef = lastClose
					}

					pf = append(pf, acc.NewPfEntry(
						e.Nick, e.Stocks, e.Price, lastClose, lastRef,
					))
				}

				ledgers = append(ledgers, acc.LedgerToJs(ledger))
				portfolios = append(portfolios, js.Wa(arr.Map(pf, acc.PfEntryToJs)))
			}

			ok = true
		})

		return cgi.Rp(ck, cgi.T{
			"ok":         js.Wb(ok),
			"ledgers":    js.Wa(ledgers),
			"portfolios": js.Wa(portfolios),
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
