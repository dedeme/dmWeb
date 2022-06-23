// Copyright 06-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Servers settings page.
package servers

import (
	"github.com/dedeme/KtMarket/data/server"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		var svs string
		var nks string
		thread.Sync(func() {
			svs = js.Ra(db.ServersTb().ReadJs())[1]
			nks = js.Ra(db.NicksTb().ReadJs())[2]
		})
		return cgi.Rp(ck, cgi.T{
			"servers": svs,
			"nicks":   nks,
		})
	case "new":
		sv := js.Rs(mrq["server"])
		var ok bool
		thread.Sync(func() {
			svsDb := db.ServersTb()
			svsTb := svsDb.Read()
			nks := db.NicksTb().Read().List
			ok = svsTb.Add(sv, nks)
			if ok {
				svsDb.Write(svsTb)
			} else {
				log.Error("Server " + sv + " already exists")
			}
		})
		return cgi.Rp(ck, cgi.T{
			"ok": js.Wb(ok),
		})
	case "del":
		id := js.Ri(mrq["id"])
		thread.Sync(func() {
			svsDb := db.ServersTb()
			svsTb := svsDb.Read()
			ok := svsTb.Del(id)
			if ok {
				svsDb.Write(svsTb)
			}
		})
		return cgi.RpEmpty(ck)
	case "modify":
		sv := server.FromJs(mrq["server"])
		var ok bool
		thread.Sync(func() {
			svsDb := db.ServersTb()
			svsTb := svsDb.Read()
			ok = svsTb.Modify(sv)
			if ok {
				svsDb.Write(svsTb)
			} else {
				log.Error("Server " + sv.ShortName + " not found")
			}
		})
		return cgi.Rp(ck, cgi.T{
			"ok": js.Wb(ok),
		})
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
