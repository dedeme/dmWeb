// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Operations performance page.
package performance

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/performanceTb"
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
			rp["operations"] = performanceTb.ReadJs(lk)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
