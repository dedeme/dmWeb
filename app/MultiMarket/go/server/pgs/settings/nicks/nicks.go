// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - nicks page.
package nicks

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
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
			model, list := nicksTb.Data(lk)
			rp["model"] = json.Wi(model)
			var nks []json.T
			for _, e := range list {
				nks = append(nks, e.ToJs())
			}
			rp["nicks"] = json.Wa(nks)
			vs := make(map[string]json.T)
			for key, value := range quotesDb.Volumes(lk, list) {
				vs[key] = json.Wd(value)
			}
			rp["volumes"] = json.Wo(vs)
		})
		return cgi.Rp(ck, rp)
	case "add":
		nickName := cgi.RqString(mrq, "nickName")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			nickId, ok := nicksTb.Add(lk, nickName)
			if ok {
				ok = quotesDb.AddNick(lk, nickName)
				if ok {
					serversTb.AddNick(lk, nickId)
				}
			}
			rp["ok"] = json.Wb(ok)
		})
		return cgi.Rp(ck, rp)
	case "del":
		id := cgi.RqInt(mrq, "id")
		sync.Run(func(lk sync.T) {
			nk, ok := nicksTb.GetNick(lk, id)
			if ok {
				quotesDb.DelNick(lk, nk.Name())
			}
			nicksTb.Del(lk, id)
			serversTb.DelNick(lk, id)
		})
		return cgi.RpEmpty(ck)
	case "setIsSel":
		id := cgi.RqInt(mrq, "id")
		value := cgi.RqBool(mrq, "value")
		sync.Run(func(lk sync.T) {
			nicksTb.SetIsSel(lk, id, value)
		})
		return cgi.RpEmpty(ck)
	case "setModel":
		id := cgi.RqInt(mrq, "id")
		sync.Run(func(lk sync.T) {
			nicksTb.SetModel(lk, id)
		})
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
