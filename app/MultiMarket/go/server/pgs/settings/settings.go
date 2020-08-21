// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main settings page.
package settings

import (
	"fmt"
	"github.com/dedeme/MultiMarket/server/pgs/settings/calendar"
	"github.com/dedeme/MultiMarket/server/pgs/settings/nicks"
	"github.com/dedeme/MultiMarket/server/pgs/settings/settings"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	source := cgi.RqString(mrq, "source")
	switch source {
	case "nicks":
		return nicks.Process(ck, mrq)
	case "calendar":
		return calendar.Process(ck, mrq)
	case "settings":
		return settings.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", source))
	}
}
