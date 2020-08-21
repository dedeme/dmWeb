// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - settings page.
package settings

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "setLang":
		lang := cgi.RqString(mrq, "lang")
		sync.Run(func(lk sync.T) { conf.SetLang(lk, lang) })
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
