// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas references test page.
package ftestsReferences

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "nickList":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			var nks []json.T
			for _, e := range nicksTb.SelectedNicks(lk) {
				nks = append(nks, json.Ws(e.Name()))
			}
			rp["nickList"] = json.Wa(nks)
		})
		return cgi.Rp(ck, rp)
	case "chartData":
		modelId := cgi.RqString(mrq, "modelId")
		nickName := cgi.RqString(mrq, "nickName")
		var params []float64
		for _, e := range mrq["params"].Ra() {
			params = append(params, e.Rd())
		}
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			opens := quotesDb.Opens(lk)
			nkOs, ok1 := opens.NickValues(nickName)
			if !ok1 {
				log.Error(lk, "Opens of "+nickName+" not found")
			}
			closes := quotesDb.Closes(lk)
			nkCs, ok2 := closes.NickValues(nickName)
			if !ok2 {
				log.Error(lk, "Opens of "+nickName+" not found")
			}
			md, ok3 := fmodels.GetModel(modelId)
			if !ok3 {
				log.Error(lk, "Model "+modelId+" not found")
			}

			rp["ok"] = json.Wb(false)
			if ok1 && ok2 && ok3 {
				dates := quotesDb.Dates(lk)
				refs := md.Refs(nkCs, params)

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
				rp["profits"] = json.Wd(md.Profits(nkOs, nkCs, params))
				rp["ok"] = json.Wb(true)
			}
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
