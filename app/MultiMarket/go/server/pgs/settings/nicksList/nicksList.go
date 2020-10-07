// Copyright 31-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nicks list page.
package nicksList

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/MultiMarket/net"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "download":
		nickId := cgi.RqInt(mrq, "nickId")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			withWarnings, withErrors := net.UpdateHistoric(lk, nickId)
			result := ""
			if withWarnings {
				result = "warning"
			} else if withErrors {
				result = "error"
			}

			rp["result"] = json.Ws(result)
		})
		return cgi.Rp(ck, rp)
	case "test":
		nickName := cgi.RqString(mrq, "nickName")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			_, withWarnings, withErrors := quotesDb.CheckQs(lk, nickName)
			result := ""
			if withWarnings {
				result = "warning"
			} else if withErrors {
				result = "error"
			}

			rp["result"] = json.Ws(result)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
