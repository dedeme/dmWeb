// Copyright 21-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investors settings page.
package investorsPg

import (
	"fmt"
	"github.com/dedeme/QMarket/db/investorsTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		lock.Run(func() {
			rp["investors"] = investorsTb.Read().ToJs()
		})
		return cgi.Rp(ck, rp)
	case "update":
		qlevel := cgi.RqInt(mrq, "qlevel")
		nickName := cgi.RqString(mrq, "nickName")
		rangeValue := cgi.RqInt(mrq, "rangeValue")
		oldValue := 0
		rp := map[string]json.T{}
		lock.Run(func() {
			changed := true
			invsTb := investorsTb.Read()
			if nickName == "" {
				invsTb.Base = rangeValue
			} else {
				oldValue = invsTb.Params[nickName][qlevel]
				invsTb.Params[nickName][qlevel] = rangeValue
			}
			investorsTb.Write(invsTb)
			investorsTb.Regularize()
			if nickName != "" {
				invsTb := investorsTb.Read()
				if invsTb.Params[nickName][qlevel] == oldValue {
					changed = false
				}
			}
			rp["changed"] = json.Wb(changed)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
