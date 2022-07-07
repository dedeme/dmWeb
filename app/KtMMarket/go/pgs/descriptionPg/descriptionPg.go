// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Description page.
package descriptionPg

import (
	"github.com/dedeme/KtMMarket/data/model"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		modelId := js.Rs(mrq["modelId"])
		md := model.FromId(modelId)
		return cgi.Rp(ck, cgi.T{
			"model": model.ToJs(md),
		})
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
