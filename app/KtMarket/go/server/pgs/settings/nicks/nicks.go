// Copyright 04-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings-Nicks page
package nicks

import (
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/quote"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		modelJs := js.Wn()
		var nks []*nick.T
		volumes := map[string]string{}

		thread.Sync(func() {
			tb := db.NicksTb().Read()
			if tb.Model != -1 {
				modelJs = js.Wi(tb.Model)
			}
			nks = tb.List
			for _, nk := range nks {
				qs, err := db.QsRead(nk.Name)
				if err != "" {
					log.Error(err)
					volumes[nk.Name] = js.Wd(0.0)
				} else {
					volumes[nk.Name] = js.Wd(quote.VolumeAvg(qs))
				}
			}
		})

		return cgi.Rp(ck, cgi.T{
			"model": modelJs,
			"nicks": js.Wa(arr.Map(nks, func(nk *nick.T) string {
				return nick.ToJs(nk)
			})),
			"volumes": js.Wo(volumes),
		})
	case "add":
		nickName := js.Rs(mrq["nickName"])

		var ok bool

		thread.Sync(func() {
			if db.QsExists(nickName) {
				nksDb := db.NicksTb()
				nksTb := nksDb.Read()
				newNksTb, nk, addOk := nksTb.Add(nickName)
				ok = addOk
				if ok {
					svsDb := db.ServersTb()
					svsTb := svsDb.Read()
					svsTb.NickAdd(nk.Id)

					nksDb.Write(newNksTb)
					svsDb.Write(svsTb)
				} else {
					log.Error("Name '" + nickName + "' duplicated")
				}
			} else {
				log.Error("Table '" + nickName + "' not found.\n" +
					"Files of new tables must be added manually to 'data/quotes'.")
			}
		})

		return cgi.Rp(ck, cgi.T{
			"ok": js.Wb(ok),
		})
	case "del":
		id := js.Ri(mrq["id"])
		thread.Sync(func() {
			nksDb := db.NicksTb()
			nksTb := nksDb.Read()
			nk, ok := nksTb.NickFromId(id)
			if ok {
				nksDb.Write(nksTb.Del(id))
				db.QsDelete(nk.Name)
			}
			svsDb := db.ServersTb()
			svsTb := svsDb.Read()
			svsTb.NickRemove(id)
			svsDb.Write(svsTb)
		})
		return cgi.RpEmpty(ck)
	case "setIsSel":
		id := js.Ri(mrq["id"])
		value := js.Rb(mrq["value"])
		thread.Sync(func() {
			nksDb := db.NicksTb()
			nksDb.Write(nksDb.Read().SetSel(id, value))
		})
		return cgi.RpEmpty(ck)
	case "setModel":
		id := js.Ri(mrq["id"])
		thread.Sync(func() {
			nksDb := db.NicksTb()
			nksDb.Write(nksDb.Read().SetModel(id))
		})
		return cgi.RpEmpty(ck)
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
