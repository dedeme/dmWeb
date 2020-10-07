// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas selection test page.
package ftestsSelection

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/db/fleas/flog"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/MultiMarket/scheduler/fleas"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "logId":
		rp := map[string]json.T{}
		rp["logId"] = json.Ws(flog.NewId())
		return cgi.Rp(ck, rp)
	case "start":
		modelId := cgi.RqString(mrq, "modelId")
		logId := cgi.RqString(mrq, "logId")
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			md, ok := fmodels.GetModel(modelId)
			if !ok {
				log.Error(lk, "Model "+modelId+" not found")
				rp["ok"] = json.Wb(false)
				return
			}
			if flog.Check(logId) {
				opens := quotesDb.Opens(lk)
				closes := quotesDb.Closes(lk)
				go fleas.Selection(opens, closes, md, logId)
				rp["ok"] = json.Wb(true)
				return
			}
			log.Error(lk, "Fleas log identifier is not valid")
			rp["ok"] = json.Wb(false)
		})
		return cgi.Rp(ck, rp)
	case "continue":
		logId := cgi.RqString(mrq, "logId")
		log, ok := flog.Read(logId)
		if !ok {
			log = json.Wn()
		}
		return cgi.Rp(ck, map[string]json.T{"log": log})
	case "stop":
		logId := cgi.RqString(mrq, "logId")
		flog.Info(logId, "Stopped")
		flog.Stop(logId)
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
