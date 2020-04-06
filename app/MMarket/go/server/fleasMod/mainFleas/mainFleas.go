// Copyright 06-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main fleas source.
package mainFleas

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ac chan func(), ck string, mrq map[string]json.T) string {
	key := cgi.RqString(mrq, "rq")
	switch key {
	case "models":
		return "ok"
	default:
		panic(fmt.Sprintf("Value of key is '%v', but it must be '%v'",
			key, "xxx"))
	}
}
