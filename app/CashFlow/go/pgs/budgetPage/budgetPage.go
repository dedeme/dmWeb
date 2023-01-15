// Copyright 29-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package budgetPage

import (
	"github.com/dedeme/CashFlow/db/hconta"
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/CashFlow/db/years/year/balance"
	"github.com/dedeme/CashFlow/db/years/year/budget"
	"github.com/dedeme/CashFlow/db/years/year/diary"
	"github.com/dedeme/CashFlow/db/years/year/plan"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/math"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		year := js.Rs(mrq["year"])
		path := years.YearPath(year)
		nYear := math.ToInt(year)
		previousPath := years.YearPath(str.Fmt("%v", nYear - 1))
		hcBalance, hcDiary := hconta.Read(year)
		rp := map[string]string{}
		rp["plan"] = plan.Read(path)
		rp["hcBalance"] = js.Wd(hcBalance)
		rp["cBalance"] = balance.Read(path)
		rp["hcDiary"] = hcDiary.ToJs()
		rp["cDiary"] = diary.Read(path)
		rp["budget"] = budget.Read(path)
		rp["previousBudget"] = budget.Read(previousPath)
		return cgi.Rp(ck, rp)
	case "updateBalance":
		year := js.Rs(mrq["year"])
		path := years.YearPath(year)
		balance.Write(path, mrq["value"])
		return cgi.RpEmpty(ck)
	default:
		panic(str.Fmt("Value of rq ('%v') is not valid", rq))
	}
}
