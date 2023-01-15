// Copyright 28-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package planPage

import (
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/CashFlow/db/years/year/diary"
	"github.com/dedeme/CashFlow/db/years/year/plan"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		year := js.Rs(mrq["year"])
		path := years.YearPath(year)
		rp := map[string]string{}
		rp["plan"] = plan.Read(path)
		rp["diary"] = diary.Read(path)
		return cgi.Rp(ck, rp)
	case "updatePlan":
		year := js.Rs(mrq["year"])
		path := years.YearPath(year)
		plan.Write(path, mrq["plan"])
		return cgi.RpEmpty(ck)
	case "updatePlanAndDiary":
		year := js.Rs(mrq["year"])
		path := years.YearPath(year)
		plan.Write(path, mrq["plan"])
		diary.Write(path, mrq["diary"])
		return cgi.RpEmpty(ck)
	default:
		panic(str.Fmt("Value of rq ('%v') is not valid", rq))
	}
}
