// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main source hub.
package mainmod

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/MMarket/db/conf"
)

func Process(ac chan func(), ck string, mrq map[string]json.T) string {
	key := cgi.RqString(mrq, "rq")
	switch key {
	case "lang":
    rp := map[string]json.T { "lang": json.Ws(conf.Lang()) }
		return cgi.Rp(ck, rp)
  case "bye" :
    return cgi.DelSession(ck, cgi.RqString(mrq, "sessionId"))
	default:
		panic(fmt.Sprintf("Value of key is '%v', but it must be '%v'",
			key, "lang | bye"))
	}
}
