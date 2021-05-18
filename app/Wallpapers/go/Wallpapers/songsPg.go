// Copyright 28-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Songs javascript page
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/db/songs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func songsProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}

		group, picture := getPicture() // pictures.go
		rp["group"] = json.Ws(group)
		rp["pict"] = json.Ws(picture)
		rp["songs"] = songs.Read()

		return cgi.Rp(ck, rp)
	case "songs":
		return cgi.Rp(ck, map[string]json.T{"songs": songs.Read()})
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
