// Copyright 04-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Dmenu request
package dmenu

import (
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "bye":
		return cgi.DelSession(ck, js.Rs(mrq["sessionId"]))
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
