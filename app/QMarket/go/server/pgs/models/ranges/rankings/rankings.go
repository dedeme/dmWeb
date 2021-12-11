// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// 'fleas/ranking' page.
package rankings

import (
	"fmt"
	"github.com/dedeme/QMarket/db/ranksDb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		qlevel := cgi.RqInt(mrq, "qlevel")
		rp := map[string]json.T{}
		lock.Run(func() {
			rp["rankings"] = ranksDb.Read(qlevel).ToJs()
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
