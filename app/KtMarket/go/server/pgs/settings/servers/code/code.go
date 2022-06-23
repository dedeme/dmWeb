// Copyright 10-May-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server code page.
package code

import (
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/net"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/sys"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "historicTest":
		serverId := js.Ri(mrq["serverId"])
		nickId := js.Ri(mrq["nickId"])
		var withError bool
		var withWarnings bool
		thread.Sync(func() {
			defer func() {
				if err := recover(); err != nil {
					log.Error(sys.Fail(str.Fmt("%v", err)))
					withError = true
				}
			}()

			nk, ok := db.NicksTb().Read().NickFromId(nickId)
			if !ok {
				log.Error(str.Fmt("Nick id %v not found", nickId))
				withError = true
				return
			}
			warns, err := net.TestHistoricConf(serverId, nk)
			if err == "" && len(warns) == 0 {
				return
			}
			if len(warns) > 0 {
				for _, e := range warns {
					log.Warning(nk.Name + "\n" + e)
				}
				withWarnings = true
			}
			if err != "" {
				log.Error(nk.Name + "\n" + err)
				withError = true
			}
		})
		return cgi.Rp(ck, cgi.T{
			"withError":    js.Wb(withError),
			"withWarnings": js.Wb(withWarnings),
		})
	default:
		panic("Value of rq ('" + rq + "') is not valid")
	}
}
