// Copyright 07-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nicks editor page.
package editor

import (
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/quote"
	"github.com/dedeme/KtMarket/data/server"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/net"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		nickId := js.Ri(mrq["nickId"])
		modelId := js.Ri(mrq["modelId"])

		var ok bool
		var qsJs []string
		var manuals int
		var mqsJs []string
		var sIdNameCodes []string

		thread.Sync(func() {
			var nk, model *nick.T

			nksTb := db.NicksTb().Read()
			nk, ok = nksTb.NickFromId(nickId)
			if !ok {
				log.Error(str.Fmt("Nick with id '%v' not found", nickId))
				return
			}

			model, ok = nksTb.NickFromId(modelId)
			if !ok {
				log.Error(str.Fmt("Nick model with id '%v' not found", nickId))
				return
			}

			qs, err := db.QsRead(nk.Name)

			if err == "" {
				qsJs = arr.Map(qs, quote.ToJs)
			} else {
				log.Error(err)
			}
			manuals = quote.Manuals(qs)

			mqs, err := db.QsRead(model.Name)

			if err == "" {
				mqsJs = arr.Map(mqs, quote.ToJs)
			} else {
				log.Error(err)
			}

			svsTb := db.ServersTb().Read()

			for _, sv := range svsTb.List {
				var nc *server.CodeT

				nc, ok = arr.Find(sv.Codes, func(c *server.CodeT) bool {
					return c.NickId == nickId
				})
				if !ok {
					log.Error(str.Fmt("Code of %v not found in %v", nk.Name, sv.Name))
					return
				}

				code, ok := nc.Code()
				if !ok {
					code = ""
				}
				sIdNameCodes = append(sIdNameCodes, js.Wa([]string{
					js.Wi(sv.Id),
					js.Ws(sv.Name),
					js.Ws(code),
				}))
			}

			ok = true
		})

		return cgi.Rp(ck, cgi.T{
			"ok":           js.Wb(ok),
			"quotes":       js.Wa(qsJs),
			"manuals":      js.Wi(manuals),
			"mquotes":      js.Wa(mqsJs),
			"sIdNameCodes": js.Wa(sIdNameCodes),
		})

	case "modifyNick":
		nickId := js.Ri(mrq["nickId"])
		name := js.Rs(mrq["name"])

		var ok = false

		thread.Sync(func() {
			nksDb := db.NicksTb()
			nksTb := nksDb.Read()
			newTb, oldName, mOk := nksTb.Modify(nickId, name)
			ok = mOk
			if ok && oldName != name {
				nksDb.Write(newTb)
				db.QsModifyNick(oldName, name)
			}
			if !ok {
				log.Error("Company not found or new name '" + name + "' duplicated")
			}
		})

		return cgi.Rp(ck, cgi.T{
			"ok": js.Wb(ok),
		})

	case "download":
		nickId := js.Ri(mrq["nickId"])

		result := ""

		thread.Sync(func() {
			nk, ok := db.NicksTb().Read().NickFromId(nickId)
			if !ok {
				log.Error(str.Fmt(
					"Editor/download: Nick %v not found", nickId,
				))
				result = "error"
				return
			}
			warnings, err := net.UpdateHistoric(nk)
			if err != "" {
				log.Error("Editor/download:\n" + err)
				result = "error"
				return
			}
			if len(warnings) != 0 {
				msg := "Editor/download:"
				for _, w := range warnings {
					msg += "\n" + w
				}
				log.Warning(msg)
				result = "warnings"
			}
			return
		})

		return cgi.Rp(ck, cgi.T{
			"result": js.Ws(result),
		})

	case "test":
		nickName := js.Rs(mrq["nickName"])
		qsTx := js.Rs(mrq["qs"])

		result := ""

		thread.Sync(func() {
			qs, err := quote.TxToQs(qsTx)
			if err != "" {
				log.Error("Editor check:\n" + err)
				result = "error"
				return
			}
			_, withWarnings, withError := db.QsCheck("Editor ("+nickName+")", qs)
			if withWarnings {
				result = "warnings"
			} else if withError {
				result = "error"
			}
		})

		return cgi.Rp(ck, cgi.T{
			"result": js.Ws(result),
		})

	case "updateCode":
		serverId := js.Ri(mrq["serverId"])
		nickId := js.Ri(mrq["nickId"])
		code := js.Rs(mrq["code"])
		thread.Sync(func() {
			svsDb := db.ServersTb()
			svsTb := svsDb.Read()
			svsTb.ModifyNickCode(serverId, nickId, code)
			svsDb.Write(svsTb)
		})
		return cgi.RpEmpty(ck)

	case "serverTests":
		serverId := js.Ri(mrq["serverId"])
		nickId := js.Ri(mrq["nickId"])

		result := ""

		thread.Sync(func() {
			defer func() {
				if err := recover(); err != nil {
					log.Error(str.Fmt("Editor/serverTests:\n%v", err))
					result = "error"
				}
			}()
			nk, ok := db.NicksTb().Read().NickFromId(nickId)
			if !ok {
				log.Error(str.Fmt("Editor/serverTests: Nick %v not found", nickId))
				result = "error"
				return
			}
			for _, sv := range db.ServersTb().Read().List {
				if sv.Id == serverId {
					if _, ok := sv.HistoricConf(); ok {
						warns, err := net.TestHistoricConf(serverId, nk)
						if err != "" {
							log.Error(str.Fmt(
								"Editor/serverTests [%v]:\n%v", sv.ShortName, err,
							))
							result = "error"
							return
						}
						if len(warns) > 0 {
							msg := str.Fmt("Editor/serverTests  [%v]:" + sv.ShortName)
							for _, w := range warns {
								msg += "\n" + w
							}
							log.Error(msg)
							result = "warnings"
						}
					}
					return
				}
			}
			log.Error(str.Fmt("Server %v not found", serverId))
			result = "error"
		})

		return cgi.Rp(ck, cgi.T{
			"result": js.Ws(result),
		})

	case "qModify":
		nickId := js.Ri(mrq["nickId"])
		qsTx := js.Rs(mrq["qs"])

		result := ""

		thread.Sync(func() {
			nk, ok := db.NicksTb().Read().NickFromId(nickId)
			if !ok {
				log.Error("Editor check: Nick identifier not found.")
				result = "error"
				return
			}

			qs, err := quote.TxToQs(qsTx)
			if err != "" {
				log.Error("Editor check (" + nk.Name + "):\n" + err)
				result = "error"
				return
			}

			qs, withWarnings, withError := db.QsCheck(nk.Name, qs)
			if withWarnings {
				result = "warnings"
				return
			} else if withError {
				result = "error"
				return
			}

			ok = db.QsWrite(nk.Name, qs)
			if !ok {
				log.Error("Fail writting " + nk.Name + ".tb")
				result = "error"
			}
		})

		return cgi.Rp(ck, cgi.T{
			"result": js.Ws(result),
		})

	case "getQuotes":
		nickName := js.Rs(mrq["nickName"])

		var ok bool
		var qsJs []string

		thread.Sync(func() {
			qs, err := db.QsRead(nickName)
			if err == "" {
				for _, q := range qs {
					qsJs = append(qsJs, quote.ToJs(q))
				}
				ok = true
			} else {
				log.Error("Editor (" + nickName + "):\n" + err)
			}
		})

		return cgi.Rp(ck, cgi.T{
			"ok":     js.Wb(ok),
			"quotes": js.Wa(qsJs),
		})

	default:
		panic("Value of rq ('" + rq + "') is not valid")
	}
}
