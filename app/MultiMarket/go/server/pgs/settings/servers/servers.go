// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Servers settings page.
package servers

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/server"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/serversTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			rp["servers"] = serversTb.ReadJs(lk)
			var nks []json.T
			for _, e := range nicksTb.Nicks(lk) {
				nks = append(nks, e.ToJs())
			}
			rp["nicks"] = json.Wa(nks)
		})
		return cgi.Rp(ck, rp)
	case "new":
		server := cgi.RqString(mrq, "server")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			rp["ok"] = json.Wb(serversTb.Add(lk, server))
		})
		return cgi.Rp(ck, rp)
	case "del":
		id := cgi.RqInt(mrq, "id")
		sync.Run(func(lk sync.T) {
			serversTb.Del(lk, id)
		})
		return cgi.RpEmpty(ck)
	case "modify":
		server := server.FromJs(mrq["server"])
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			rp["ok"] = json.Wb(serversTb.Modify(lk, server))
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
