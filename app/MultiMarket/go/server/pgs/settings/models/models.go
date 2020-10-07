// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Manager models page.
package models

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/db/managersTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		managerIx := cgi.RqInt(mrq, "managerIx")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			rp["managers"] = json.Wi(cts.Managers)
			var mds []json.T
			for _, e := range fmodels.List() {
				mds = append(mds, e.ToJs())
			}
			rp["models"] = json.Wa(mds)
			rp["manager"] = managersTb.Read(lk)[managerIx].ToJsClient()
		})
		return cgi.Rp(ck, rp)
	case "update":
		managerIx := cgi.RqInt(mrq, "managerIx")
		nickName := cgi.RqString(mrq, "nickName")
		modelId := cgi.RqString(mrq, "modelId")
		var params []float64
		for _, e := range mrq["params"].Ra() {
			params = append(params, e.Rd())
		}
		md, ok := fmodels.GetModel(modelId)
		if ok {
			sync.Run(func(lk sync.T) {
				if nickName == "" {
					managersTb.SetBase(lk, managerIx, md, params)
					return
				}
				managersTb.SetNick(lk, managerIx, nickName, md, params)
			})
		}
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
