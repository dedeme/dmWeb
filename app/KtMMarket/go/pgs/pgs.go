// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pages hub.
package pgs

import (
	"github.com/dedeme/KtMMarket/cts"
	"github.com/dedeme/KtMMarket/pgs/changePassPg"
	"github.com/dedeme/KtMMarket/pgs/cosPg"
	"github.com/dedeme/KtMMarket/pgs/descriptionPg"
	"github.com/dedeme/KtMMarket/pgs/historicPg"
	"github.com/dedeme/KtMMarket/pgs/hotPg"
	"github.com/dedeme/KtMMarket/pgs/mainPg"
	"github.com/dedeme/KtMMarket/pgs/operationsPg"
	"github.com/dedeme/KtMMarket/pgs/resultsPg"
	"github.com/dedeme/KtMMarket/pgs/simulationsPg"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/cryp"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/sys"
)

func Process(rq string) (rp string) {
	defer func() {
		if err := recover(); err != nil {
			rp = sys.Fail(str.Fmt("%v", err))
			ix1 := str.Index(rp, ":\n")
			if ix1 != -1 {
				ix2 := str.IndexFrom(rp, "runtime/panic.go", ix1)
				if ix2 != -1 {
					ix2 = str.IndexFrom(rp, "\n", ix2)
				}
				if ix2 != -1 {
					rp = rp[:ix1+2] + rp[ix2+1:]
				}
			}
		}
	}()

	ix := str.Index(rq, ":")

	// CONNECTION --------------------------------------------------------------

	if ix == -1 {
		return cgi.Connect(rq)
	}

	// AUTHENTICATION ----------------------------------------------------------

	if ix == 0 {
		key := cryp.Key(cts.AppName, cgi.Klen)
		data := cryp.Decode(key, rq[1:])
		ps := str.Split(data, ":")
		return cgi.Authentication(key, ps[0], ps[1], ps[2] == "1")
	}

	// NORMAL DATA -------------------------------------------------------------

	sessionId := rq[:ix]
	var conKey string
	rest := rq[ix+1:]
	ix = str.Index(rest, ":")
	if ix != -1 {
		conKey = rest[:ix]
		rest = rest[ix+1:]
	}
	comKey, ok := cgi.GetComKey(sessionId, conKey)
	if ok {
		j := cryp.Decode(comKey, rest)
		data := js.Ro(j)
		return processHub(comKey, data)
	} else {
		return cgi.RpExpired()
	}
}

func processHub(ck string, rq cgi.T) string {
	source := js.Rs(rq["source"])
	switch source {
	case "Main":
		return mainPg.Process(ck, rq)
	case "Description":
		return descriptionPg.Process(ck, rq)
	case "ResultsPg":
		return resultsPg.Process(ck, rq)
	case "SimulationsPg":
		return simulationsPg.Process(ck, rq)
	case "HotPg":
		return hotPg.Process(ck, rq)
	case "CosPg":
		return cosPg.Process(ck, rq)
	case "HistoricPg":
		return historicPg.Process(ck, rq)
	case "OperationsPg":
		return operationsPg.Process(ck, rq)
	case "ChangePass":
		return changePassPg.Process(ck, rq)
	default:
		panic("Value of source (" + source + ") is not valid")
	}
}
