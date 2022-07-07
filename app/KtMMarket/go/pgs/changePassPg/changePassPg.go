// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Description page.
package changePassPg

import (
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "changePass":
		user := js.Rs(mrq["user"])
		old := js.Rs(mrq["old"])
		new := js.Rs(mrq["new"])
		return cgi.ChangePass(ck, user, old, new)
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
