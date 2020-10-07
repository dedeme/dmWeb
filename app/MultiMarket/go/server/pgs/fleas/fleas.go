// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas page.
package fleas

import (
	"fmt"
	"github.com/dedeme/MultiMarket/server/pgs/fleas/charts"
	fleas2 "github.com/dedeme/MultiMarket/server/pgs/fleas/fleas"
	"github.com/dedeme/MultiMarket/server/pgs/fleas/ftestsOrders"
	"github.com/dedeme/MultiMarket/server/pgs/fleas/ftestsReferences"
	"github.com/dedeme/MultiMarket/server/pgs/fleas/ftestsSelection"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "source")
	switch rq {
	case "fleas":
		return fleas2.Process(ck, mrq)
	case "ftests/selection":
		return ftestsSelection.Process(ck, mrq)
	case "ftests/references":
		return ftestsReferences.Process(ck, mrq)
	case "ftests/orders":
		return ftestsOrders.Process(ck, mrq)
	case "charts":
		return charts.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
