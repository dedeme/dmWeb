// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// General ranking page.
package ranking

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/fleas/rankingTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	source := cgi.RqString(mrq, "source")
	switch source {
	case "ranking":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			var rk []json.T
			for _, e := range rankingTb.Read(lk) {
				rk = append(rk, e.ToJsClient())
			}
			rp["ranking"] = json.Wa(rk)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", source))
	}
}
