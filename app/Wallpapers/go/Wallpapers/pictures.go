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
	"math/rand"
	"strconv"
)

func getPicture() (group, picture string) {
	group = sels.GetGroup()
	picture = sels.GetPict()

	pictures := picts.Read(group)
	now := date.Now()
	dt := now.String() +
		fmt.Sprintf("%v:%v", now.Hour(), now.Minute()/PICTURES_TIME)
	if picture == "" ||
		!pict.Contains(pictures, picture) ||
		dt != sels.GetPictDate() {

		group = strconv.Itoa(rand.Intn(PICTURE_GROUPS))
		pictures = picts.Read(group)
		pictures, picture = pict.Next(pictures)
		if picture == "" { // All the pictures has been shown
			pictures = pict.ResetSights(pictures)
			pictures, picture = pict.Next(pictures)
		}
		picts.Write(group, pictures)
		sels.SetGroup(group)
		sels.SetPict(picture)
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
		rp["pict"] = json.Ws(pict)
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
