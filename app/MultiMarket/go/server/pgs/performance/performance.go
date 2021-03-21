// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Operations performance page.
package performance

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/performance"
	"github.com/dedeme/MultiMarket/data/acc"
	"github.com/dedeme/MultiMarket/db/performanceTb"
  "github.com/dedeme/MultiMarket/db/acc/diariesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

type operationT struct {
	nick          string
	date          string
	isSell        bool
	stocks        int
	expectedPrice float64
	actualPrice   float64
}

func (o *operationT) toJs() json.T {
	return json.Wa([]json.T{
		json.Ws(o.nick),
		json.Ws(o.date),
		json.Wb(o.isSell),
		json.Wi(o.stocks),
		json.Wd(o.expectedPrice),
		json.Wd(o.actualPrice),
	})
}

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			opsJs := performanceTb.ReadJs(lk)
			opJs := opsJs.Ra()

			diff := float64(0)
			if len(opJs) > 0 {
				firstDate := performance.FromJs(opJs[0]).Date()
        years := diariesDb.Years(lk)
        endYears := 1
        if len(years) > 1 {
          endYears = 2
        }

        for i := 0; i < endYears; i++ {
          y := years[i]
          allJs := diariesDb.ReadAllJs(lk, y)
          for _, js := range allJs.Ra() {
            ann := acc.AnnotationFromJs(js)
            date := ann.Date()
            if date > firstDate {
              if amount, _ , ok := ann.Operation().Nd(); ok {
                diff -= amount
              } else if amount, _ , ok := ann.Operation().Pd(); ok {
                diff += amount
              }
            }
          }
        }
			}

			rp["operations"] = opsJs
      rp["diff"] = json.Wd(diff)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
