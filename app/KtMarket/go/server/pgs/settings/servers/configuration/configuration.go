// Copyright 09-May-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server configuration page.
package configuration

import (
	"github.com/dedeme/KtMarket/data/nick"
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
	case "dailyTest":
		serverId := js.Ri(mrq["serverId"])
		var ok bool
		thread.Sync(func() {
			defer func() {
				if err := recover(); err != nil {
					log.Error(sys.Fail(str.Fmt("%v", err)))
					ok = false
				}
			}()

			warns, err := net.TestDailyConf(serverId)
			if err == "" && len(warns) == 0 {
				ok = true
				return
			}
			if len(warns) > 0 {
				for _, e := range warns {
					log.Warning(e)
				}
				ok = true
			}
			if err != "" {
				log.Error(sys.Fail(err))
				ok = false
			}
		})
		return cgi.Rp(ck, cgi.T{
			"ok": js.Wb(ok),
		})
	case "historicTest":
		serverId := js.Ri(mrq["serverId"])
		nickId := js.Ri(mrq["nickId"])
		var ok bool
		thread.Sync(func() {
			defer func() {
				if err := recover(); err != nil {
					log.Error(sys.Fail(str.Fmt("%v", err)))
					ok = false
				}
			}()

			var nk *nick.T
			nk, ok = db.NicksTb().Read().NickFromId(nickId)
			if !ok {
				log.Error(str.Fmt("Nick id %v not found", nickId))
				return
			}
			warns, err := net.TestHistoricConf(serverId, nk)
			if err == "" && len(warns) == 0 {
				ok = true
				return
			}
			if len(warns) > 0 {
				for _, e := range warns {
					log.Warning(nk.Name + "\n" + e)
				}
				ok = true
			}
			if err != "" {
				log.Error(nk.Name + "\n" + err)
				ok = false
			}
		})
		return cgi.Rp(ck, cgi.T{
			"ok": js.Wb(ok),
		})
	default:
		panic("Value of rq ('" + rq + "') is not valid")
	}
}
