// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Flea charts page.
package charts

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "assets":
		modelId := cgi.RqString(mrq, "modelId")
		param := mrq["param"].Rd()
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			opens := quotesDb.Opens(lk)
			closes := quotesDb.Closes(lk)

			md, ok := fmodels.GetModel(modelId)
			if !ok {
				log.Error(lk, "Model "+modelId+" not found")
				rp["ok"] = json.Wb(false)
				return
			}

			var dates []json.T
			for _, e := range quotesDb.Dates(lk) {
				dates = append(dates, json.Ws(e))
			}
			rp["dates"] = json.Wa(dates)

			var assets []json.T
			for _, e := range md.HistoricAssets(opens, closes, param) {
				assets = append(assets, json.Wd(e))
			}
			rp["assets"] = json.Wa(assets)
			rp["ok"] = json.Wb(true)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
