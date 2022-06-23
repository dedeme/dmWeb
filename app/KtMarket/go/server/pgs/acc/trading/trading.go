// Copyright 20-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// acc/trading page
package trading

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/acc"
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

		var operations string
		var rebuys []*nick.NameStrT
		thread.Sync(func() {
			now := time.Now()
			for i := 0; i < cts.Investors; i++ {
				anns := diariesDb.ReadAnnotations(i)
				_, _, lastOps, errs := acc.Settlement(anns)
				for _, err := range errs {
					log.Error(err)
				}
				for k, v := range lastOps {
					if rs, ok := v.Profits(); ok {
						if rs < 0 {
							tm := time.FromStr(v.Date)
							if time.DfDays(now, tm) < 63 {
								rebuys = append(rebuys, nick.NewNameStr(k, v.Date))
							}
						}
					}
				}
			}

			operations = db.InvOperationsTb().ReadJs()
		})

		return cgi.Rp(ck, cgi.T{
			"bet":        js.Wd(cts.Bet),
			"rebuys":     js.Wa(arr.Map(rebuys, nick.NameStrToJs)),
			"operations": operations,
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
