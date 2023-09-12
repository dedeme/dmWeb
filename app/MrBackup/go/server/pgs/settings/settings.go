// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings page.
package settings

import (
	"fmt"
	"github.com/dedeme/MrBackup/db/conf"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "setLang":
		lang := js.Rs(mrq["lang"])
		conf.SetLang(lang)
		return cgi.RpEmpty(ck)
	case "changePass":
		user := js.Rs(mrq["user"])
		new := js.Rs(mrq["new"])
		old := js.Rs(mrq["old"])
		return cgi.ChangePass(ck, user, old, new)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
