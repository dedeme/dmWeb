// Copyright 17-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package clubsPg

import (
	"fmt"
	"github.com/dedeme/Bet1x2/db/allClubs"
	"github.com/dedeme/Bet1x2/db/yearDb/clubs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		year := cgi.RqString(mrq, "year")
		rp := map[string]json.T{}
		rp["allClubs"] = allClubs.Read()
		rp["clubs"] = clubs.Read(year)
		return cgi.Rp(ck, rp)
	case "updateAllClubs":
		allClubs.Write(mrq["allClubs"])
		return cgi.RpEmpty(ck)
	case "updateClubs":
		year := cgi.RqString(mrq, "year")
		clubs.Write(year, mrq["clubs"])
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
