// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main page.
package mainPg

import (
	"github.com/dedeme/KtMMarket/data/model"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		ls := arr.Map(model.List(), func(md *model.T) string {
			return md.Id
		})
		return cgi.Rp(ck, cgi.T{
			"mainModelId": js.Ws(ls[0]),
			"modelIds":    js.Wa(arr.Map(ls, js.Ws)),
		})
	case "close":
		sessionId := js.Rs(mrq["sessionId"])
		cgi.DelSession(ck, sessionId)
		return cgi.RpEmpty(ck)
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
