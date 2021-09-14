// Copyright 29-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package budgetEdit

import (
	"fmt"
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/CashFlow/db/years/year/budget"
	"github.com/dedeme/CashFlow/db/years/year/diary"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "updateBudget":
		year := cgi.RqString(mrq, "year")
		path := years.YearPath(year)
		budget.Write(path, mrq["budget"])
		return cgi.RpEmpty(ck)
	case "updateDiary":
		year := cgi.RqString(mrq, "year")
		path := years.YearPath(year)
		diary.Write(path, mrq["diary"])
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
