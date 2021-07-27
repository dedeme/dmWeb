// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investor models page.
package models

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea"
	"github.com/dedeme/MultiMarket/data/flea/eFlea"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/db/investorsTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		investorIx := cgi.RqInt(mrq, "investorIx")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			rp["investors"] = json.Wi(cts.Investors)
			var mds []json.T
			for _, e := range fmodels.List() {
				mds = append(mds, e.ToJs())
			}
			rp["models"] = json.Wa(mds)
			inv := investorsTb.Read(lk)[investorIx]
			rp["investor"] = inv.ToJsClient()
			closes := quotesDb.Closes(lk)
			opens := quotesDb.Opens(lk)
			eflea := eFlea.New(flea.New(inv.Base.Param()))
			eFlea.Evaluate(inv.Base.Model(), opens, closes, []*eFlea.T{eflea})
			rp["eflea"] = eflea.ToJs()
		})
		return cgi.Rp(ck, rp)
	case "update":
		investorIx := cgi.RqInt(mrq, "investorIx")
		nickName := cgi.RqString(mrq, "nickName")
		modelId := cgi.RqString(mrq, "modelId")
		param := mrq["param"].Rd()
		md, ok := fmodels.GetModel(modelId)
		if ok {
			sync.Run(func(lk sync.T) {
				if nickName == "" {
					investorsTb.SetBase(lk, investorIx, md, param)
					return
				}
				investorsTb.SetNick(lk, investorIx, nickName, md, param)
			})
		}
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
