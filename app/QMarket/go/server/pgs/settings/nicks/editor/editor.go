// Copyright 31-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nicks editor page.
package editor

import (
	"fmt"
	"github.com/dedeme/QMarket/data/quote"
	"github.com/dedeme/QMarket/data/server"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/db/serversTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/QMarket/net"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		nickId := cgi.RqInt(mrq, "nickId")
		modelId := cgi.RqInt(mrq, "modelId")
		rp := map[string]json.T{}
		lock.Run(func() {
			nksTb := nicksTb.Read()
			nk, ok := nksTb.NickFromId(nickId)
			if !ok {
				logTb.Error(fmt.Sprintf("Nick with id '%v' not found", nickId))
				rp["ok"] = json.Wb(false)
				return
			}

			model, ok := nksTb.NickFromId(modelId)
			if !ok {
				logTb.Error(fmt.Sprintf("Nick model with id '%v' not found", nickId))
				rp["ok"] = json.Wb(false)
				return
			}

			qs, err := quotesDb.Read(nk.Name())

			var qsJs []json.T
			if err == nil {
				for _, q := range qs {
					qsJs = append(qsJs, q.ToJs())
				}
			} else {
				logTb.Error(err.Error())
			}
			rp["quotes"] = json.Wa(qsJs)
			rp["manuals"] = json.Wi(quote.Manuals(qs))

			mqs, err := quotesDb.Read(model.Name())

			var mqsJs []json.T
			if err == nil {
				for _, q := range mqs {
					mqsJs = append(mqsJs, q.ToJs())
				}
			} else {
				logTb.Error(err.Error())
			}
			rp["mquotes"] = json.Wa(mqsJs)

			svsTb := serversTb.Read()
			var sIdNameCodes []json.T
			for _, sv := range svsTb.List() {
				var nc *server.CodeT
				for _, e := range sv.Codes() {
					if e.NickId() == nickId {
						nc = e
						break
					}
				}
				if nc == nil {
					logTb.Error(fmt.Sprintf(
						"Code of %v not found in %v", nk.Name(), sv.Name(),
					))
					rp["ok"] = json.Wb(false)
					return
				}

				code, ok := nc.Code()
				if !ok {
					code = ""
				}
				sIdNameCodes = append(sIdNameCodes, json.Wa([]json.T{
					json.Wi(sv.Id()),
					json.Ws(sv.Name()),
					json.Ws(code),
				}))
			}
			rp["sIdNameCodes"] = json.Wa(sIdNameCodes)

			rp["ok"] = json.Wb(true)
		})
		return cgi.Rp(ck, rp)
	case "modifyNick":
		nickId := cgi.RqInt(mrq, "nickId")
		name := cgi.RqString(mrq, "name")
		rp := map[string]json.T{}
		lock.Run(func() {
			nksTb := nicksTb.Read()
			oldName, ok := nksTb.Modify(nickId, name)
			if ok && oldName != "" {
				nicksTb.Write(nksTb)
				quotesDb.ModifyNickName(oldName, name)
			}
			rp["ok"] = json.Wb(ok)
		})
		return cgi.Rp(ck, rp)
	case "download":
		nickId := cgi.RqInt(mrq, "nickId")
		rp := map[string]json.T{}
		result := ""
		lock.Run(func() {
			nk, ok := nicksTb.Read().NickFromId(nickId)
			if !ok {
				logTb.Error(fmt.Sprintf(
					"Editor/download: Nick %v not found", nickId,
				))
				result = "error"
				return
			}
			warnings, err := net.UpdateHistoric(nk)
			if err != nil {
				logTb.Error("Editor/download:\n" + err.Error())
				result = "error"
				return
			}
			if len(warnings) != 0 {
				msg := "Editor/download:"
				for _, w := range warnings {
					msg += "\n" + w.Error()
				}
				logTb.Info(msg)
				result = "warnings"
			}
		})
		rp["result"] = json.Ws(result)
		return cgi.Rp(ck, rp)
	case "test":
		qsTx := cgi.RqString(mrq, "qs")
		rp := map[string]json.T{}
		result := ""
		lock.Run(func() {
			qs, err := quote.TxToQs(qsTx)
			if err != nil {
				logTb.Error("Editor check:\n" + err.Error())
				result = "error"
				return
			}
			_, withWarnings, withError := quotesDb.Check("Editor", qs)
			if withWarnings {
				result = "warnings"
			} else if withError {
				result = "error"
			}
		})
		rp["result"] = json.Ws(result)
		return cgi.Rp(ck, rp)
	case "updateCode":
		serverId := cgi.RqInt(mrq, "serverId")
		nickId := cgi.RqInt(mrq, "nickId")
		code := cgi.RqString(mrq, "code")
		lock.Run(func() {
			svsTb := serversTb.Read()
			svsTb.ModifyNickCode(serverId, nickId, code)
			serversTb.Write(svsTb)
		})
		return cgi.RpEmpty(ck)
	case "serverTests":
		serverId := cgi.RqInt(mrq, "serverId")
		nickId := cgi.RqInt(mrq, "nickId")
		result := ""
		rp := map[string]json.T{}
		lock.Run(func() {
			defer func() {
				if err := recover(); err != nil {
					logTb.Error(fmt.Sprintf("Editor/serverTests:\n%v", err))
					rp["result"] = "error"
				}
			}()
			nk, ok := nicksTb.Read().NickFromId(nickId)
			if !ok {
				logTb.Error(fmt.Sprintf(
					"Editor/serverTests: Nick %v not found", nickId,
				))
				result = "error"
				return
			}
			for _, sv := range serversTb.Read().List() {
				if sv.Id() == serverId {
					if _, ok := sv.HistoricConf(); ok {
						warns, err := net.TestHistoricConf(serverId, nk)
						if err != nil {
							logTb.Error(fmt.Sprintf(
								"Editor/serverTests [%v]:\n%v", sv.ShortName(), err.Error(),
							))
							result = "error"
							return
						}
						if len(warns) > 0 {
							msg := fmt.Sprintf("Editor/serverTests  [%v]:" + sv.ShortName())
							for _, w := range warns {
								msg += "\n" + w.Error()
							}
							logTb.Error(msg)
							result = "warnings"
						}
					}
					return
				}
			}
			logTb.Error(fmt.Sprintf("Server %v not found", serverId))
			result = "error"
		})
		rp["result"] = json.Ws(result)
		return cgi.Rp(ck, rp)
	case "qModify":
		nickId := cgi.RqInt(mrq, "nickId")
		qsTx := cgi.RqString(mrq, "qs")
		rp := map[string]json.T{}
		result := ""
		lock.Run(func() {
			nk, ok := nicksTb.Read().NickFromId(nickId)
			if !ok {
				logTb.Error("Editor check: Nick identifier not found.")
				result = "error"
				return
			}

			qs, err := quote.TxToQs(qsTx)
			if err != nil {
				logTb.Error("Editor check:\n%" + err.Error())
				result = "error"
				return
			}

			qs, withWarnings, withError := quotesDb.Check(nk.Name(), qs)
			if withWarnings {
				result = "warnings"
				return
			} else if withError {
				result = "error"
				return
			}

			ok = quotesDb.Write(nk.Name(), qs)
			if !ok {
				logTb.Error("Fail writting " + nk.Name() + ".tb")
				result = "error"
			}
		})
		rp["result"] = json.Ws(result)
		return cgi.Rp(ck, rp)

	case "getQuotes":
		nickName := cgi.RqString(mrq, "nickName")
		rp := map[string]json.T{}
		lock.Run(func() {
			qs, err := quotesDb.Read(nickName)
			var qsJs []json.T
			ok := false
			if err == nil {
				for _, q := range qs {
					qsJs = append(qsJs, q.ToJs())
				}
				ok = true
			} else {
				logTb.Error("Editor:\n" + err.Error())
			}
			rp["ok"] = json.Wb(ok)
			rp["quotes"] = json.Wa(qsJs)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
