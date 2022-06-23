// Copyright 05-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings-Settings page
package settings

import (
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "setLang":
		lang := js.Rs(mrq["lang"])
		thread.Sync(func() {
			confTb := db.ConfTb()
			conf := confTb.Read()
			conf.Lang = lang
			confTb.Write(conf)
		})
		return cgi.RpEmpty(ck)
	case "changePass":
		user := js.Rs(mrq["user"])
		new := js.Rs(mrq["new"])
		old := js.Rs(mrq["old"])
		return cgi.ChangePass(ck, user, old, new)
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
