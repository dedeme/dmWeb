// Copyright 20-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// acc/trading page
package trading

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/acc"
	"github.com/dedeme/KtMarket/data/quote"
	"github.com/dedeme/KtMarket/data/invOperation"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/db/acc/diariesDb"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/thread"
	"github.com/dedeme/ktlib/time"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":

		var operationsJs string
		var rebuys []*nick.NameStrT
    portfolios := make([][]*acc.PfEntryT, cts.Investors)
		thread.Sync(func() {
			operationsJs = db.InvOperationsTb().ReadJs()
      ops := db.InvOperationsTb().Read().Operations
			now := time.Now()
			for i := 0; i < cts.Investors; i++ {
				anns := diariesDb.ReadAnnotations(i)
				_, pf, lastOps, errs := acc.Settlement(anns)
				for _, err := range errs {
					log.Error(err)
				}
				for k, v := range lastOps {
					if rs, ok := v.Profits(); ok {
						if rs < cts.RebuyLimit {
							tm := time.FromStr(v.Date)
							if time.DfDays(now, tm) < 63 {
								rebuys = append(rebuys, nick.NewNameStr(k, v.Date))
							}
						}
					}
				}

        pfEntries := arr.Map(arr.Filter(pf, func(e *acc.PfEntryT) bool {
          _, ok := arr.Find(ops, func (op *invOperation.T) bool {
            return op.Nick == e.Nick && op.Investor == i
          })
          return ok
        }), func(e *acc.PfEntryT) *acc.PfEntryT {
          nk, ok := db.NicksTb().Read().NickFromName(e.Nick)
          if !ok {
            log.Error("Nick " + e.Nick + " not found")
            return e
          }
          _, closes, err := db.CurrentCloses(nk)
          if err != ""{
            log.Error(err)
            return e
          }
          lastClose := quote.LastValue(closes)
          return acc.NewPfEntry(e.Nick, e.Stocks, e.Price, lastClose, 0.0)
        })
        portfolios[i] = pfEntries
			}
		})

		return cgi.Rp(ck, cgi.T{
			"bet":        js.Wd(cts.Bet),
			"rebuys":     js.Wa(arr.Map(rebuys, nick.NameStrToJs)),
			"operations": operationsJs,
      "portfolios": js.Wa(
        arr.Map(portfolios, func(pfEntries []*acc.PfEntryT) string {
          return js.Wa(arr.Map(pfEntries, acc.PfEntryToJs))
        })), // value Ref == 0.0
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
