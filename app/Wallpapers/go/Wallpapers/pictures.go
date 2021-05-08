// Copyright 28-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Pictures javascript page
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/data/pict"
	"github.com/dedeme/Wallpapers/db/picts"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
)

func nextPicture() string {
	ps := picts.Read()
	p := sels.GetPict()
	now := date.Now()
	dt := now.String() +
		fmt.Sprintf("%v:%v", now.Hour(), now.Minute()/PICTURES_TIME)
	if p == "" || !pict.Contains(ps, p) || dt != sels.GetPictDate() {
		newPs, newP := pict.Next(ps)
		if newP == "" { // All the pictures has been sown
			ps = pict.ResetSights(ps)
			newPs, newP = pict.Next(ps)
		}
		picts.Write(newPs)
		sels.SetPict(newP)
		sels.SetPictDate(dt)
		p = newP
	}
	return p
}

func picturesProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		rp["pict"] = json.Ws(nextPicture())
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
