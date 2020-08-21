// Copyright 17-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Home page.
package home

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "log":
		lg := map[string]json.T{}
		sync.Run(func(lk sync.T) { lg["log"] = log.Read(lk) })
		return cgi.Rp(ck, lg)
	case "reset":
		sync.Run(func(lk sync.T) { log.Reset(lk) })
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
