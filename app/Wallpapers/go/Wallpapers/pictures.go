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

func getPicture() (group string, picture *pict.T) {
	group = sels.GetGroup()
	pictureId := sels.GetPict()

	pictures := picts.Read(group)
	if pictureId != "" {
		picture = pict.Get(pictures, pictureId)
	}

	now := date.Now()
	dt := now.String() +
		fmt.Sprintf("%v:%v", now.Hour(), now.Minute()/sels.GetPictTime())
	if picture == nil || dt != sels.GetPictDate() {
		group, picture = picts.Next()
		sels.SetGroup(group)
		sels.SetPict(picture.Id())
		sels.SetPictDate(dt)
	}

	return
}

func picturesProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		group, pict := getPicture()
		rp["group"] = json.Ws(group)
		rp["pict"] = pict.ToJs()
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
