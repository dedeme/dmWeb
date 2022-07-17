// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting data group hub.
package acc

import (
	"github.com/dedeme/KtMarket/server/pgs/acc/balance"
	"github.com/dedeme/KtMarket/server/pgs/acc/companies"
	"github.com/dedeme/KtMarket/server/pgs/acc/jail"
	"github.com/dedeme/KtMarket/server/pgs/acc/profits"
	"github.com/dedeme/KtMarket/server/pgs/acc/speedometers"
	"github.com/dedeme/KtMarket/server/pgs/acc/trading"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, rq cgi.T) string {
	source := js.Rs(rq["source"])
	switch source {
	case "companies":
		return companies.Process(ck, rq)
	case "balance":
		return balance.Process(ck, rq)
	case "trading":
		return trading.Process(ck, rq)
	case "profits":
		return profits.Process(ck, rq)
	case "speedometers":
		return speedometers.Process(ck, rq)
	case "jail":
		return jail.Process(ck, rq)
	default:
		panic("Value of source (" + source + ") is not valid")
	}
}
