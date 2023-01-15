// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package changePass

import (
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "change":
		user := js.Rs(mrq["user"])
		old := js.Rs(mrq["old"])
		new := js.Rs(mrq["new"])
		return cgi.ChangePass(ck, user, old, new)
	default:
		panic(str.Fmt("Value of rq ('%v') is not valid", rq))
	}
}
