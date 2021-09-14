// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package year

import (
	"fmt"
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		var yearsJs []json.T
		for _, y := range years.List() {
			yearsJs = append(yearsJs, json.Ws(y))
		}
		rp := map[string]json.T{}
		rp["years"] = json.Wa(yearsJs)
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
