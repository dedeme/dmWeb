// Copyright 21-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main daily charts page.
package daily

import (
	daily2 "github.com/dedeme/KtMarket/server/pgs/daily/daily"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, rq cgi.T) string {
	source := js.Rs(rq["source"])
	switch source {
	case "daily":
		return daily2.Process(ck, rq)
	default:
		panic("Value of source (" + source + ") is not valid")
	}
}
