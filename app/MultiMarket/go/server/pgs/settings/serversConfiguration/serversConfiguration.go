// Copyright 31-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server configuration page.
package serversConfiguration

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/MultiMarket/net"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "dailyTest":
		serverId := cgi.RqInt(mrq, "serverId")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			defer func() {
				if err := recover(); err != nil {
					rp["ok"] = json.Wb(false)
				}
			}()

			rp["ok"] = json.Wb(net.TestDailyConf(lk, serverId))
		})
		return cgi.Rp(ck, rp)
	case "historicTest":
		serverId := cgi.RqInt(mrq, "serverId")
		nickId := cgi.RqInt(mrq, "nickId")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			defer func() {
				if err := recover(); err != nil {
					rp["ok"] = json.Wb(false)
				}
			}()

			nick, ok := nicksTb.GetNick(lk, nickId)
			if !ok {
				log.Error(lk, fmt.Sprintf("Nick id %v not found", nickId))
				rp["ok"] = json.Wb(false)
				return
			}
			rp["ok"] = json.Wb(net.TestHistoricConf(lk, serverId, nick))
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
