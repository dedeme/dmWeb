// Copyright 21-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main settings page.
package settings

import (
	"fmt"
	"github.com/dedeme/QMarket/server/pgs/settings/acc"
	"github.com/dedeme/QMarket/server/pgs/settings/acc/accAll"
	"github.com/dedeme/QMarket/server/pgs/settings/acc/accEditor"
	"github.com/dedeme/QMarket/server/pgs/settings/calendarPg"
	"github.com/dedeme/QMarket/server/pgs/settings/investorsPg"
	"github.com/dedeme/QMarket/server/pgs/settings/nicks/editor"
	"github.com/dedeme/QMarket/server/pgs/settings/nicks/list"
	"github.com/dedeme/QMarket/server/pgs/settings/nicks/nicks"
	"github.com/dedeme/QMarket/server/pgs/settings/servers/code"
	"github.com/dedeme/QMarket/server/pgs/settings/servers/servers"
	//"github.com/dedeme/MultiMarket/server/pgs/settings/serversConfiguration"
	"github.com/dedeme/QMarket/server/pgs/settings/settings"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	source := cgi.RqString(mrq, "source")
	switch source {
	case "nicks":
		return nicks.Process(ck, mrq)
	case "nicks/list":
		return list.Process(ck, mrq)
	case "nicks/editor":
		return editor.Process(ck, mrq)
	case "servers":
		return servers.Process(ck, mrq)
	case "investors":
		return investorsPg.Process(ck, mrq)
	//	case "servers/configuration":
	//		return serversConfiguration.Process(ck, mrq)
	case "servers/code":
		return serversCode.Process(ck, mrq)
	case "calendar":
		return calendarPg.Process(ck, mrq)
	case "acc":
		return acc.Process(ck, mrq)
	case "acc/editor":
		return accEditor.Process(ck, mrq)
	case "acc/all":
		return accAll.Process(ck, mrq)
	case "settings":
		return settings.Process(ck, mrq)
		//	case "models":
		//		return models.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", source))
	}
}
