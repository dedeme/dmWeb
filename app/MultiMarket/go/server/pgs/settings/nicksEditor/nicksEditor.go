// Copyright 31-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nicks editor page.
package nicksEditor

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/nick"
	"github.com/dedeme/MultiMarket/data/quote"
	"github.com/dedeme/MultiMarket/data/server"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/db/serversTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/MultiMarket/net"
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
		sync.Run(func(lk sync.T) {
			nk, ok := nicksTb.GetNick(lk, nickId)
			if !ok {
				log.Error(lk, fmt.Sprintf("Nick with id '%v' not found", nickId))
				rp["ok"] = json.Wb(false)
				return
			}

			var model *nick.T
			model, ok = nicksTb.GetNick(lk, modelId)
			if !ok {
				log.Error(lk, fmt.Sprintf("Nick model with id '%v' not found", nickId))
				rp["ok"] = json.Wb(false)
				return
			}

			qs := quotesDb.Read(lk, nk.Name())
			if len(qs) == 0 {
				log.Error(lk, fmt.Sprintf(
          "'%v'.tb not found, empty or in bad condition",
          nk.Name()))
				rp["ok"] = json.Wb(false)
				return
			}

			var qsJs []json.T
			for _, q := range qs {
				qsJs = append(qsJs, q.ToJs())
			}
			rp["quotes"] = json.Wa(qsJs)
			rp["manuals"] = json.Wi(quote.Manuals(qs))

			mqs := quotesDb.Read(lk, model.Name())
			if len(mqs) == 0 {
				log.Error(lk, fmt.Sprintf(
          "'%v'.tb not found, empty or in bad condition",
          model.Name()))
				rp["ok"] = json.Wb(false)
				return
			}

			var mqsJs []json.T
			for _, q := range mqs {
				mqsJs = append(mqsJs, q.ToJs())
			}
			rp["mquotes"] = json.Wa(mqsJs)

			var sIdNameCodes []json.T
			for _, sv := range serversTb.Read(lk) {
				var nc *server.CodeT
				for _, e := range sv.Codes() {
					if e.NickId() == nickId {
						nc = e
						break
					}
				}
				if nc == nil {
					log.Error(lk, fmt.Sprintf(
						"Code of %v not found in %v",
						nk.Name(), sv.Name(),
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
		sync.Run(func(lk sync.T) {
			oldName, ok := nicksTb.SetName(lk, nickId, name)
			if ok {
				quotesDb.ModifyNickName(lk, oldName, name)
			}
			rp["ok"] = json.Wb(ok)
		})
		return cgi.Rp(ck, rp)
	case "download":
		nickId := cgi.RqInt(mrq, "nickId")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			withWarnings, withErrors := net.UpdateHistoric(lk, nickId)
			result := ""
			if withWarnings {
				result = "warning"
			} else if withErrors {
				result = "error"
			}
			rp["result"] = json.Ws(result)
		})
		return cgi.Rp(ck, rp)
	case "test":
		qsTx := cgi.RqString(mrq, "qs")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			_, withWarnings, withErrors := quotesDb.Check(lk, qsTx)
			result := ""
			if withWarnings {
				result = "warning"
			} else if withErrors {
				result = "error"
			}
			rp["result"] = json.Ws(result)
		})
		return cgi.Rp(ck, rp)
	case "updateCode":
		serverId := cgi.RqInt(mrq, "serverId")
		nickId := cgi.RqInt(mrq, "nickId")
		code := cgi.RqString(mrq, "code")
		sync.Run(func(lk sync.T) {
			serversTb.ModifyNickCode(lk, serverId, nickId, code)
		})
		return cgi.RpEmpty(ck)
	case "serverTests":
		serverId := cgi.RqInt(mrq, "serverId")
		nickId := cgi.RqInt(mrq, "nickId")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			defer func() {
				if err := recover(); err != nil {
					rp["ok"] = json.Wb(false)
				}
			}()

			nk, ok := nicksTb.GetNick(lk, nickId)
			if !ok {
				log.Error(lk, fmt.Sprintf("Nick with id '%v' not found", nickId))
				rp["ok"] = json.Wb(false)
				return
			}

			if serversTb.IsDefinedHistoricConf(lk, serverId) {
				rp["ok"] = json.Wb(net.TestHistoricConf(lk, serverId, nk))
			} else {
				rp["ok"] = json.Wb(true)
			}
		})
		return cgi.Rp(ck, rp)
	case "qModify":
		nickId := cgi.RqInt(mrq, "nickId")
		qsTx := cgi.RqString(mrq, "qs")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			qs, withWarnings, withErrors := quotesDb.Check(lk, qsTx)
			if withErrors {
				rp["result"] = json.Ws("error")
				return
			}
			ok := quotesDb.SetQuotes(lk, nickId, qs)
			if !ok {
				rp["result"] = json.Ws("error")
				return
			}
			if withWarnings {
				rp["result"] = json.Ws("warning")
				return
			}
			rp["result"] = json.Ws("")
		})
		return cgi.Rp(ck, rp)
	case "getQuotes":
		nickName := cgi.RqString(mrq, "nickName")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			qs := quotesDb.Read(lk, nickName)

			var qsJs []json.T
			for _, q := range qs {
				qsJs = append(qsJs, q.ToJs())
			}
			rp["quotes"] = json.Wa(qsJs)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
