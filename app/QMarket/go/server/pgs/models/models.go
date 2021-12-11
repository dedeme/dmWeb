// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Models hub.
package models

import (
	"fmt"
	"github.com/dedeme/QMarket/server/pgs/models/charts/charts"
	"github.com/dedeme/QMarket/server/pgs/models/charts/companies"
	models2 "github.com/dedeme/QMarket/server/pgs/models/models"
	"github.com/dedeme/QMarket/server/pgs/models/tests/orders"
	"github.com/dedeme/QMarket/server/pgs/models/tests/references"
	"github.com/dedeme/QMarket/server/pgs/models/ranges/ranges"
	"github.com/dedeme/QMarket/server/pgs/models/ranges/rankings"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "source")
	switch rq {
	case "models":
		return models2.Process(ck, mrq)
		case "tests/references":
			return references.Process(ck, mrq)
	case "tests/orders":
		return orders.Process(ck, mrq)
	case "charts":
		return charts.Process(ck, mrq)
	case "companies":
		return companies.Process(ck, mrq)
  case "ranges":
		return ranges.Process(ck, mrq)
  case "rankings":
  return rankings.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
