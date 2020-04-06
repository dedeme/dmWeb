// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Home source hub.
package home

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/MMarket/db/log"
)

func Process(ac chan func(), ck string, mrq map[string]json.T) string {
	key := cgi.RqString(mrq, "rq")
	switch key {
	case "log":
    rp := map[string]json.T { "log": log.Read() }
		return cgi.Rp(ck, rp)
	case "reset":
    log.Reset()
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of key is '%v', but it must be '%v'",
			key, "log | reset"))
	}
}
