// Copyright 29-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package budgetPage

import (
	"fmt"
	"github.com/dedeme/CashFlow/db/hconta"
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/CashFlow/db/years/year/balance"
	"github.com/dedeme/CashFlow/db/years/year/budget"
	"github.com/dedeme/CashFlow/db/years/year/diary"
	"github.com/dedeme/CashFlow/db/years/year/plan"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"strconv"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		year := cgi.RqString(mrq, "year")
		path := years.YearPath(year)
		previousYear, _ := strconv.Atoi(year)
		previousPath := years.YearPath(strconv.Itoa(previousYear))
		hcBalance, hcDiary := hconta.Read(year)
		rp := map[string]json.T{}
		rp["plan"] = plan.Read(path)
		rp["hcBalance"] = json.Wd(hcBalance)
		rp["cBalance"] = balance.Read(path)
		rp["hcDiary"] = hcDiary.ToJs()
		rp["cDiary"] = diary.Read(path)
		rp["budget"] = budget.Read(path)
		rp["previousBudget"] = budget.Read(previousPath)
		return cgi.Rp(ck, rp)
	case "updateBalance":
		year := cgi.RqString(mrq, "year")
		path := years.YearPath(year)
		balance.Write(path, mrq["value"])
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
