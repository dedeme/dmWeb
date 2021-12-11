// Copyright 21-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Servers codes settings page.
package serversCode

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
	case "historicTest":
		serverId := cgi.RqInt(mrq, "serverId")
		nickId := cgi.RqInt(mrq, "nickId")
		rp := map[string]json.T{}
		lock.Run(func() {
			rp["withWarnings"] = json.Wb(false)
			defer func() {
				if err := recover(); err != nil {
					rp["error"] = json.Wb(true)
				}
			}()

			nick, ok := nicksTb.Read().NickFromId(nickId)
			if !ok {
				logTb.Error(fmt.Sprintf("Nick id %v not found", nickId))
				rp["withError"] = json.Wb(true)
				return
			}
			warns, err := net.TestHistoricConf(serverId, nick)
			if err == nil && len(warns) == 0 {
				rp["withError"] = json.Wb(false)
				return
			}
			if len(warns) > 0 {
				for _, e := range warns {
					logTb.Info(e.Error())
				}
				rp["withWarnings"] = json.Wb(true)
			}
			if err != nil {
				logTb.Error(err.Error())
				rp["withError"] = json.Wb(true)
			}
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
