// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// acc/profits page
package profits

import (
	"github.com/dedeme/KtMarket/db/acc/profitsDb"
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
			data = profitsDb.ReadJs()
		})

		return cgi.Rp(ck, cgi.T{
			"data": data,
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
