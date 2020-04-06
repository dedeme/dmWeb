// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings hub.
package settingsHub

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/MMarket/server/settingsMod/settings"
	"github.com/dedeme/MMarket/server/settingsMod/chpass"
	"github.com/dedeme/MMarket/server/settingsMod/calendar"
)

func Process(ac chan func(), ck string, mrq map[string]json.T) string {
	key := cgi.RqString(mrq, "source")
	switch key {
	case "Settings":
		return settings.Process(ac, ck, mrq)
  case "ChangePass":
    return chpass.Process(ac, ck, mrq)
  case "Calendar":
    return calendar.Process(ac, ck, mrq)
	default:
		panic(fmt.Sprintf("Value of key is '%v', but it must be '%v'",
			key, "Settings | ChangePass | Calendar"))
	}
}
