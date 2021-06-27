// Copyright 25-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Info widget
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/db/picts"
	"github.com/dedeme/Wallpapers/db/songs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func infoProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "changePictLevel":
		group := cgi.RqString(mrq, "group")
		id := cgi.RqString(mrq, "id")
		level := cgi.RqInt(mrq, "level")
		picts.SetLevel(group, id, level)
		return cgi.RpEmpty(ck)
	case "changeSongLevel":
		group := cgi.RqString(mrq, "group")
		id := cgi.RqString(mrq, "id")
		level := cgi.RqInt(mrq, "level")
		songs.SetLevel(group, id, level)
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
