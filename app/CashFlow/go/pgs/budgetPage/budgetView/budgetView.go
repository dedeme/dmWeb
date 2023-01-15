// Copyright 29-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package budgetView

import (
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/CashFlow/db/years/year/diary"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/str"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "updateDiary":
		year := js.Rs(mrq["year"])
		path := years.YearPath(year)
		diary.Write(path, mrq["diary"])
		return cgi.RpEmpty(ck)
	default:
		panic(str.Fmt("Value of rq ('%v') is not valid", rq))
	}
}
