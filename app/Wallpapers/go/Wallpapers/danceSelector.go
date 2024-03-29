// Copyright 17-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Songs management javascript page
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/db/danceSongs"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func danceSelectorProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		groups := danceSongs.GetGroups()
		var groupsJs []json.T
		for _, g := range groups {
			groupsJs = append(groupsJs, json.Ws(g))
		}
		group := sels.GetDanceSelectorGroup()
		if group == "" {
			group = groups[0]
		}
		rp["groups"] = json.Wa(groupsJs)
		rp["group"] = json.Ws(group)
		rp["songs"] = danceSongs.ReadJs(group)

		return cgi.Rp(ck, rp)
	case "changeGroup":
		sels.SetDanceSelectorGroup(cgi.RqString(mrq, "group"))
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
