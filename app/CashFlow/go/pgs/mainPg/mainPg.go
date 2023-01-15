// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package mainPg

import (
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/str"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "close":
		sessionId := js.Rs(mrq["sessionId"])
		return cgi.DelSession(ck, sessionId)
	default:
		panic(str.Fmt("Value of rq ('%v') is not valid", rq))
	}
}
