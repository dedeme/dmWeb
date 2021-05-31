// Copyright 25-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Times page
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func timesProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		rp["pict"] = json.Wi(sels.GetPictTime())
		rp["shortDance"] = json.Wi(sels.GetShortDanceTime())
		rp["longDance"] = json.Wi(sels.GetLongDanceTime())
		return cgi.Rp(ck, rp)
	case "update":
		key := cgi.RqString(mrq, "key")
		value := cgi.RqInt(mrq, "value")
		if key == "pict" {
			sels.SetPictTime(value)
		} else if key == "shortDance" {
			sels.SetShortDanceTime(value)
		} else {
			sels.SetLongDanceTime(value)
		}
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
