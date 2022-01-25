// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas references test page.
package references

import (
	"fmt"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "nickList":
		rp := map[string]json.T{}
		lock.Run(func() {
			var nks []json.T
			for _, e := range nicksTb.Read().SelectedNicks() {
				nks = append(nks, json.Ws(e.Name()))
			}
			rp["nickList"] = json.Wa(nks)
		})
		return cgi.Rp(ck, rp)
	case "chartData":
		qlevel := cgi.RqInt(mrq, "qlevel")
		nickName := cgi.RqString(mrq, "nickName")
		paramId := mrq["paramId"].Ri()
		rp := map[string]json.T{}
		lock.Run(func() {
			opens := quotesDb.Opens()
			nkOs, ok1 := opens.NickValues(nickName)
			if !ok1 {
				logTb.Error("Opens of " + nickName + " not found")
			}
			closes := quotesDb.Closes()
			nkCs, ok2 := closes.NickValues(nickName)
			if !ok2 {
				logTb.Error("Opens of " + nickName + " not found")
			}

			rp["ok"] = json.Wb(false)
			if ok1 && ok2 {
				dates := quotesDb.Dates()
				md := model.New(qlevel, paramId)
				refs := md.Refs(nkCs)

				var datesJs []json.T
				for _, e := range dates {
					datesJs = append(datesJs, json.Ws(e))
				}
				var opensJs []json.T
				for _, e := range nkOs {
					opensJs = append(opensJs, json.Wd(e[0]))
				}
				var closesJs []json.T
				for _, e := range nkCs {
					closesJs = append(closesJs, json.Wd(e[0]))
				}
				var refsJs []json.T
				for _, e := range refs {
					refsJs = append(refsJs, json.Wd(e))
				}
				rp["dates"] = json.Wa(datesJs)
				rp["opens"] = json.Wa(opensJs)
				rp["closes"] = json.Wa(closesJs)
				rp["refs"] = json.Wa(refsJs)
				rp["profits"] = json.Wd(md.Profits(nkOs, nkCs))
				rp["ok"] = json.Wb(true)
			}
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
