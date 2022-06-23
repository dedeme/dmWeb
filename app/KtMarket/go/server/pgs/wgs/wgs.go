// Copyright 03-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Witgets hub
package wgs

import (
	"github.com/dedeme/KtMarket/server/pgs/wgs/dmenu"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, rq cgi.T) string {
	source := js.Rs(rq["source"])
	switch source {
	case "dmenu":
		return dmenu.Process(ck, rq)
	default:
		panic("Value of source (" + source + ") is not valid")
	}
}
