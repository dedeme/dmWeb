// Copyright 14-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Models hub
package models

import (
	"github.com/dedeme/KtMarket/server/pgs/models/companies"
	"github.com/dedeme/KtMarket/server/pgs/models/modelsPg"
	"github.com/dedeme/KtMarket/server/pgs/models/tests/orders"
	"github.com/dedeme/KtMarket/server/pgs/models/tests/references"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, rq cgi.T) string {
	source := js.Rs(rq["source"])
	switch source {
	case "models":
		return modelsPg.Process(ck, rq)
	case "companies":
		return companies.Process(ck, rq)
	case "tests/references":
		return references.Process(ck, rq)
	case "tests/orders":
		return orders.Process(ck, rq)
	default:
		panic("Value of source (" + source + ") is not valid")
	}
}
