// Copyright 31-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server configuration page.
package configuration

import (
	"fmt"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/QMarket/net"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "dailyTest":
		serverId := cgi.RqInt(mrq, "serverId")
		rp := map[string]json.T{}
		lock.Run(func() {
			defer func() {
				if err := recover(); err != nil {
					rp["ok"] = json.Wb(false)
				}
			}()

			warns, err := net.TestDailyConf(serverId)
			if err == nil && len(warns) == 0 {
				rp["ok"] = json.Wb(true)
				return
			}
			if len(warns) > 0 {
				for _, e := range warns {
					logTb.Info(e.Error())
				}
				rp["ok"] = json.Wb(true)
			}
			if err != nil {
				logTb.Error(err.Error())
				rp["ok"] = json.Wb(false)
			}
		})
		return cgi.Rp(ck, rp)
	case "historicTest":
		serverId := cgi.RqInt(mrq, "serverId")
		nickId := cgi.RqInt(mrq, "nickId")
		rp := map[string]json.T{}
		lock.Run(func() {
			defer func() {
				if err := recover(); err != nil {
					rp["ok"] = json.Wb(false)
				}
			}()

			nick, ok := nicksTb.Read().NickFromId(nickId)
			if !ok {
				logTb.Error(fmt.Sprintf("Nick id %v not found", nickId))
				rp["ok"] = json.Wb(false)
				return
			}
			warns, err := net.TestHistoricConf(serverId, nick)
			if err == nil && len(warns) == 0 {
				rp["ok"] = json.Wb(true)
				return
			}
			if len(warns) > 0 {
				for _, e := range warns {
					logTb.Info(e.Error())
				}
				rp["ok"] = json.Wb(true)
			}
			if err != nil {
				logTb.Error(err.Error())
				rp["ok"] = json.Wb(false)
			}
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
