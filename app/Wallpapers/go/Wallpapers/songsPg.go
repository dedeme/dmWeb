// Copyright 28-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Songs javascript page
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/Wallpapers/data/song"
	"github.com/dedeme/Wallpapers/db/songs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func getCurrentSong ()(s string, lapse float64) {
  s = sels.GetSong()
  lapse = sels.GetLapse()
  ss := songs.Read()

  if s == "" || !song.IsValid(ss, s, lapse) {
    ss, s = song.Next(ss)
    if s == "" { // All the songs has been heard
      ss = song.ResetSights(ss)
			ss, s = song.Next(ss)
    }
    lapse = 0

    songs.Write(ss)
    sels.SetSong(s)
    sels.SetLapse(lapse)
  }

  return
}

func updateSong (lapse float64)(s string) {
  s = sels.GetSong()
  sels.SetLapse(lapse)

  currentSong, _ := getCurrentSong()
  if currentSong == s {
    return
  }

  s = currentSong
  return
}

func songsProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}

    picture := nextPicture() // pictures.go
		rp["pict"] = json.Ws(picture)

    song, lapse := getCurrentSong()
    rp["song"] = json.Ws(song)
    rp["lapse"] = json.Wd(lapse)

		return cgi.Rp(ck, rp)
  case "update":
    lapse := cgi.RqDouble(mrq, "lapse")
		rp := map[string]json.T{}

    picture := nextPicture() // pictures.go
		rp["pict"] = json.Ws(picture)

    song := updateSong(lapse)
    rp["song"] = json.Ws(song)

		return cgi.Rp(ck, rp)

	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
