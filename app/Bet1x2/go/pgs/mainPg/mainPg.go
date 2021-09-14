// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package mainPg

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "close":
		sessionId := cgi.RqString(mrq, "sessionId")
		return cgi.DelSession(ck, sessionId)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
