// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Home page.
package home

import (
	"fmt"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "log":
		rp := map[string]json.T{}
		lock.Run(func() { rp["log"] = logTb.ReadJs() })

		return cgi.Rp(ck, rp)
	case "reset":
		lock.Run(func() { logTb.Reset() })
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
