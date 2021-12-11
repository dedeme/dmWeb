// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - settings page.
package settings

import (
	"fmt"
	"github.com/dedeme/QMarket/db/confTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "setLang":
		lang := cgi.RqString(mrq, "lang")
		lock.Run(func() { confTb.SetLang(lang) })
		return cgi.RpEmpty(ck)
	case "changePass":
		user := cgi.RqString(mrq, "user")
		new := cgi.RqString(mrq, "new")
		old := cgi.RqString(mrq, "old")
		return cgi.ChangePass(ck, user, old, new)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
