// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main widgets page.
package wgs

import (
	"fmt"
	"github.com/dedeme/QMarket/server/pgs/wgs/dmenu"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	source := cgi.RqString(mrq, "source")
	switch source {
	case "dmenu":
		return dmenu.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", source))
	}
}
