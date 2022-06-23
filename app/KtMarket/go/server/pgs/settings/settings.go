// Copyright 04-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings hub
package settings

import (
	"github.com/dedeme/KtMarket/server/pgs/settings/acc"
	"github.com/dedeme/KtMarket/server/pgs/settings/acc/accAll"
	"github.com/dedeme/KtMarket/server/pgs/settings/acc/accEditor"
	"github.com/dedeme/KtMarket/server/pgs/settings/calendar"
	"github.com/dedeme/KtMarket/server/pgs/settings/investorsPg"
	"github.com/dedeme/KtMarket/server/pgs/settings/nicks"
	"github.com/dedeme/KtMarket/server/pgs/settings/nicks/editor"
	"github.com/dedeme/KtMarket/server/pgs/settings/nicks/list"
	"github.com/dedeme/KtMarket/server/pgs/settings/servers"
	"github.com/dedeme/KtMarket/server/pgs/settings/servers/code"
	"github.com/dedeme/KtMarket/server/pgs/settings/servers/configuration"
	setts "github.com/dedeme/KtMarket/server/pgs/settings/settings"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, rq cgi.T) string {
	source := js.Rs(rq["source"])
	switch source {
	case "nicks":
		return nicks.Process(ck, rq)
	case "nicks/list":
		return list.Process(ck, rq)
	case "nicks/editor":
		return editor.Process(ck, rq)
	case "servers":
		return servers.Process(ck, rq)
	case "servers/configuration":
		return configuration.Process(ck, rq)
	case "servers/code":
		return code.Process(ck, rq)
	case "acc":
		return acc.Process(ck, rq)
	case "acc/editor":
		return accEditor.Process(ck, rq)
	case "acc/all":
		return accAll.Process(ck, rq)
	case "investorsPg":
		return investorsPg.Process(ck, rq)
	case "calendar":
		return calendar.Process(ck, rq)
	case "settings":
		return setts.Process(ck, rq)
	default:
		panic("Value of source (" + source + ") is not valid")
	}
}
