// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package changePass

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "change":
		user := cgi.RqString(mrq, "user")
		old := cgi.RqString(mrq, "old")
		new := cgi.RqString(mrq, "new")
		return cgi.ChangePass(ck, user, old, new)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
