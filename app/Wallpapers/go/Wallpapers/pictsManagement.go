// Copyright 28-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pictures management javascript page
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/db/picts"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func pictsManagementProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		group := sels.GetPictsGroup()
		rp["groups"] = json.Wi(PICTURE_GROUPS)
		rp["group"] = json.Ws(group)
		rp["picts"] = picts.ReadJs(group)
		rp["page"] = json.Wi(sels.GetPictsPage())

		return cgi.Rp(ck, rp)
	case "setLevel":
		group := cgi.RqString(mrq, "group")
		id := cgi.RqString(mrq, "id")
		level := cgi.RqInt(mrq, "level")
		picts.SetLevel(group, id, level)
		return cgi.RpEmpty(ck)
	case "setPage":
		page := cgi.RqInt(mrq, "page")
		sels.SetPictsPage(page)
		return cgi.RpEmpty(ck)
	case "setGroup":
		group := cgi.RqString(mrq, "group")
		sels.SetPictsGroup(group)
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
