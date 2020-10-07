// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main settings page.
package settings

import (
	"fmt"
	"github.com/dedeme/MultiMarket/server/pgs/settings/acc"
	"github.com/dedeme/MultiMarket/server/pgs/settings/accAll"
	"github.com/dedeme/MultiMarket/server/pgs/settings/accEditor"
	"github.com/dedeme/MultiMarket/server/pgs/settings/calendar"
	"github.com/dedeme/MultiMarket/server/pgs/settings/models"
	"github.com/dedeme/MultiMarket/server/pgs/settings/nicks"
	"github.com/dedeme/MultiMarket/server/pgs/settings/nicksEditor"
	"github.com/dedeme/MultiMarket/server/pgs/settings/nicksList"
	"github.com/dedeme/MultiMarket/server/pgs/settings/servers"
	"github.com/dedeme/MultiMarket/server/pgs/settings/serversCode"
	"github.com/dedeme/MultiMarket/server/pgs/settings/serversConfiguration"
	"github.com/dedeme/MultiMarket/server/pgs/settings/settings"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	source := cgi.RqString(mrq, "source")
	switch source {
	case "nicks":
		return nicks.Process(ck, mrq)
	case "nicks/list":
		return nicksList.Process(ck, mrq)
	case "nicks/editor":
		return nicksEditor.Process(ck, mrq)
	case "servers":
		return servers.Process(ck, mrq)
	case "servers/configuration":
		return serversConfiguration.Process(ck, mrq)
	case "servers/code":
		return serversCode.Process(ck, mrq)
	case "calendar":
		return calendar.Process(ck, mrq)
	case "acc":
		return acc.Process(ck, mrq)
	case "acc/editor":
		return accEditor.Process(ck, mrq)
	case "acc/all":
		return accAll.Process(ck, mrq)
	case "settings":
		return settings.Process(ck, mrq)
	case "models":
		return models.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", source))
	}
}
