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
    var pfEntries []*acc.PfEntryT
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

        es := arr.Map(arr.Filter(pf, func(e *acc.PfEntryT) bool {
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
          closes, err := db.CurrentCloses(nk)
          if err != ""{
            log.Error(err)
            return e
          }
          lastClose := quote.LastValue(closes)
          newe := acc.NewPfEntry(e.Nick, e.Stocks, e.Price, lastClose, 0.0)

          e2, ok := arr.Find(pfEntries, func(e2 *acc.PfEntryT) bool {
            return e2.Nick == e.Nick
          })
          if ok {
            stocks := e2.Stocks + e.Stocks
            price := (float64(e2.Stocks) * e2.Price + float64(e.Stocks) * e.Price) /
              float64(e2.Stocks + e.Stocks)
            newe = acc.NewPfEntry(e.Nick, stocks, price, lastClose, 0.0)
          }

          return newe
        })
        pfEntries = append(pfEntries, es...)
			}
		})

		return cgi.Rp(ck, cgi.T{
			"bet":        js.Wd(cts.Bet),
			"rebuys":     js.Wa(arr.Map(rebuys, nick.NameStrToJs)),
			"operations": operationsJs,
      "pfEntries": js.Wa(arr.Map(pfEntries, acc.PfEntryToJs)), // value Ref == 0.0
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
