// Copyright 28-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package planPage

import (
	"fmt"
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/CashFlow/db/years/year/diary"
	"github.com/dedeme/CashFlow/db/years/year/plan"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		year := cgi.RqString(mrq, "year")
		path := years.YearPath(year)
		rp := map[string]json.T{}
		rp["plan"] = plan.Read(path)
		rp["diary"] = diary.Read(path)
		return cgi.Rp(ck, rp)
	case "updatePlan":
		year := cgi.RqString(mrq, "year")
		path := years.YearPath(year)
		plan.Write(path, mrq["plan"])
		return cgi.RpEmpty(ck)
	case "updatePlanAndDiary":
		year := cgi.RqString(mrq, "year")
		path := years.YearPath(year)
		plan.Write(path, mrq["plan"])
		diary.Write(path, mrq["diary"])
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
