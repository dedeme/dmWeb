// Copyright 30-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Songs management javascript page
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/Wallpapers/db/songs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func songsManagementProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}

		rp["songs"] = songs.ReadJs()
		rp["page"] = json.Wi(sels.GetPictsPage())

		return cgi.Rp(ck, rp)
	case "setLevel":
		id := cgi.RqString(mrq, "id")
		level := cgi.RqInt(mrq, "level")
		songs.SetLevel(id, level)
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
