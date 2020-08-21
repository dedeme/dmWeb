// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - nicks page.
package nicks

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			model, list := nicksTb.Data(lk)
			rp["model"] = json.Wi(model)
			var nks []json.T
			for _, e := range list {
				nks = append(nks, e.ToJs())
			}
			rp["nicks"] = json.Wa(nks)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
