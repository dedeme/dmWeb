// Copyright 19-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - nicks page.
package nicks

import (
	"fmt"
	"github.com/dedeme/QMarket/data/quote"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/quotesDb"
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
			nkTb := nicksTb.Read()
			md, ok := nkTb.Model()
			if ok {
				rp["model"] = json.Wi(md)
			} else {
				rp["model"] = json.Wn()
			}
			nks := nkTb.List()
			var nksJs []json.T
			vols := make(map[string]json.T)
			for _, n := range nks {
				nksJs = append(nksJs, n.ToJs())
				qs, err := quotesDb.Read(n.Name())
				if err == nil {
					vols[n.Name()] = json.Wd(quote.VolumeAvg(qs))
				} else {
					logTb.Error(err.Error())
					vols[n.Name()] = json.Wd(0.0)
				}
			}
			rp["nicks"] = json.Wa(nksJs)
			rp["volumes"] = json.Wo(vols)
		})
		return cgi.Rp(ck, rp)
	case "add":
		nickName := cgi.RqString(mrq, "nickName")
		rp := map[string]json.T{}
		lock.Run(func() {
			ok := quotesDb.Exists(nickName)
			if ok {
				nkTb := nicksTb.Read()
				newNkTb, nk, ok := nkTb.Add(nickName)
				if ok {
					svTb := serversTb.Read()
					svTb.NickAdd(nk.Id())

					nicksTb.Write(newNkTb)
					serversTb.Write(svTb)
				} else {
					logTb.Error("Name '" + nickName + "' duplicated")
				}
			} else {
				logTb.Error("Table '" + nickName + "' not found.\n" +
					"Files of new tables must be added manually to 'data/quotes'.")
			}
			rp["ok"] = json.Wb(ok)
		})
		return cgi.Rp(ck, rp)
	case "del":
		id := cgi.RqInt(mrq, "id")
		lock.Run(func() {
			nkTb := nicksTb.Read()
			nk, ok := nkTb.NickFromId(id)
			if ok {
				nicksTb.Write(nkTb.Del(id))
				quotesDb.Delete(nk.Name())
			}
			svTb := serversTb.Read()
			svTb.NickRemove(id)
			serversTb.Write(svTb)
		})
		return cgi.RpEmpty(ck)
	case "setIsSel":
		id := cgi.RqInt(mrq, "id")
		value := cgi.RqBool(mrq, "value")
		lock.Run(func() {
			nicksTb.Write(nicksTb.Read().SetSel(id, value))
		})
		return cgi.RpEmpty(ck)
	case "setModel":
		id := cgi.RqInt(mrq, "id")
		lock.Run(func() {
			nicksTb.Write(nicksTb.Read().SetModel(id))
		})
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
