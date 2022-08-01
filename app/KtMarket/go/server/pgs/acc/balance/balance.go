// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// acc/balance page
package balance

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/acc"
	"github.com/dedeme/KtMarket/data/invOperation"
	"github.com/dedeme/KtMarket/data/nick"
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
			invOps := db.InvOperationsTb().Read().Operations
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

					_, allCloses, err := db.CurrentCloses(nk)
					if err != "" {
						log.Error(err)
						continue
					}
					closes := arr.Drop(allCloses, len(allCloses)-cts.ReferenceQuotes)
					lastClose := closes[len(closes)-1]

					cought := arr.Anyf(invOps, func(iv *invOperation.T) bool {
						return iv.Investor == i && iv.Nick == nk.Name
					})
					var lastRef float64
					if cought {
						lastRef = lastClose
					} else {
						lastRef = db.LastRef(inv, nk.Name, closes, allCloses)
						if e.Stocks > 0 && lastRef > lastClose {
							lastRef = lastClose
						}
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
