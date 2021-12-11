// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main daily charts page.
package daily

import (
	"fmt"
	daily2 "github.com/dedeme/QMarket/server/pgs/daily/daily"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "source")
	switch rq {
	case "daily":
		return daily2.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
