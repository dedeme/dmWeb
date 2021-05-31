// Copyright 28-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Songs javascript page
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/data/song"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/Wallpapers/db/songs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func getSong() (group string, s *song.T) {
	group = sels.GetSongGroup()
	songId := sels.GetSong()

	ss := songs.Read(group)
	if songId != "" {
		s = song.Get(ss, songId)
	}

	if s == nil {
		group, s = songs.Next()
		sels.SetSongGroup(group)
		sels.SetSong(s.Id())
	}

	return

}

func songsProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}

		group, picture := getPicture() // pictures.go
		rp["group"] = json.Ws(group)
		rp["pict"] = picture.ToJs()

		groupSong, s := getSong()
		rp["groupSong"] = json.Ws(groupSong)
		rp["song"] = s.ToJs()

		return cgi.Rp(ck, rp)
	case "pictData":
		rp := map[string]json.T{}

		group, picture := getPicture() // pictures.go
		rp["group"] = json.Ws(group)
		rp["pict"] = picture.ToJs()

		return cgi.Rp(ck, rp)
	case "setLapse":
		songs.SetLapse(
			cgi.RqString(mrq, "group"),
			cgi.RqString(mrq, "song"),
			cgi.RqDouble(mrq, "lapse"),
		)
		return cgi.RpEmpty(ck)
	case "newSong":
		rp := map[string]json.T{}
		currentGroup := sels.GetSongGroup()
		currentSong := sels.GetSong()
		songs.SetLapse(currentGroup, currentSong, 0.0)
		group, s := songs.Next()
		sels.SetSongGroup(group)
		sels.SetSong(s.Id())
		rp["group"] = json.Ws(group)
		rp["song"] = s.ToJs()
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
