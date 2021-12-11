// Copyright 21-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Servers settings page.
package servers

import (
	"fmt"
	"github.com/dedeme/QMarket/data/server"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/serversTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		lock.Run(func() {
			var svs []json.T
			for _, sv := range serversTb.Read().List() {
				svs = append(svs, sv.ToJs())
			}
			rp["servers"] = json.Wa(svs)
			var nks []json.T
			for _, nk := range nicksTb.Read().List() {
				nks = append(nks, nk.ToJs())
			}
			rp["nicks"] = json.Wa(nks)
		})
		return cgi.Rp(ck, rp)
	case "new":
		server := cgi.RqString(mrq, "server")
		rp := map[string]json.T{}
		lock.Run(func() {
			svsTb := serversTb.Read()
			nicks := nicksTb.Read().List()
			ok := svsTb.Add(server, nicks)
			if ok {
				serversTb.Write(svsTb)
			}
			rp["ok"] = json.Wb(ok)
		})
		return cgi.Rp(ck, rp)
	case "del":
		id := cgi.RqInt(mrq, "id")
		lock.Run(func() {
			svsTb := serversTb.Read()
			ok := svsTb.Del(id)
			if ok {
				serversTb.Write(svsTb)
			}
		})
		return cgi.RpEmpty(ck)
	case "modify":
		server := server.FromJs(mrq["server"])
		rp := map[string]json.T{}
		lock.Run(func() {
			svsTb := serversTb.Read()
			ok := svsTb.Modify(server)
			if ok {
				serversTb.Write(svsTb)
			}
			rp["ok"] = json.Wb(ok)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
