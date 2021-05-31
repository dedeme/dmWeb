// Copyright 28-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pictures javascript page
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func danceProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		isShort := cgi.RqBool(mrq, "isShort")
		rp := map[string]json.T{}
		group, pict := getPicture() // in pictures.go
		rp["group"] = json.Ws(group)
		rp["pict"] = pict.ToJs()
		var duration float64
		if isShort {
			duration = float64(sels.GetShortDanceTime()) * 60000.0
		} else {
			duration = float64(sels.GetLongDanceTime()) * 60000.0
		}
		rp["duration"] = json.Wd(duration)
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
