// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting data group hub.
package acc

import (
	"fmt"
	"github.com/dedeme/MultiMarket/server/pgs/acc/balance"
	"github.com/dedeme/MultiMarket/server/pgs/acc/companies"
	"github.com/dedeme/MultiMarket/server/pgs/acc/profits"
	"github.com/dedeme/MultiMarket/server/pgs/acc/trading"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "source")
	switch rq {
	case "companies":
		return companies.Process(ck, mrq)
	case "balance":
		return balance.Process(ck, mrq)
	case "trading":
		return trading.Process(ck, mrq)
	case "profits":
		return profits.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
