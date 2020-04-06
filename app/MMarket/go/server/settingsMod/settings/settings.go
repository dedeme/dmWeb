// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings source hub.
package settings

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/MMarket/db/conf"
)

func Process(ac chan func(), ck string, mrq map[string]json.T) string {
	key := cgi.RqString(mrq, "rq")
	switch key {
	case "setLang":
    conf.SetLang(cgi.RqString(mrq, "lang"))
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of key is '%v', but it must be '%v'",
			key, "setLang"))
	}
}
