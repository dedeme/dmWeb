// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// acc/jail page
package jail

import (
	"github.com/dedeme/KtMarket/db/acc/jailLossesDb"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":

		var data string
		thread.Sync(func() {
			data = jailLossesDb.ReadJs()
		})

		return cgi.Rp(ck, cgi.T{
			"data": data,
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
