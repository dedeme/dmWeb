// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package year

import (
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		var yearsJs []string
		for _, y := range years.List() {
			yearsJs = append(yearsJs, js.Ws(y))
		}
		rp := map[string]string{}
		rp["years"] = js.Wa(yearsJs)
		return cgi.Rp(ck, rp)
	default:
		panic(str.Fmt("Value of rq ('%v') is not valid", rq))
	}
}
