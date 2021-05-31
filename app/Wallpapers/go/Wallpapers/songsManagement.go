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
		groups := songs.GetGroups()
		var groupsJs []json.T
		for _, g := range groups {
			groupsJs = append(groupsJs, json.Ws(g))
		}
		rp["groups"] = json.Wa(groupsJs)
		group := sels.GetSongsGroup()
		if group == "" {
			group = groups[0]
		}
		rp["group"] = json.Ws(group)
		rp["songs"] = songs.ReadJs(group)
		rp["songGroup"] = json.Ws(sels.GetSongGroup())
		rp["song"] = json.Ws(sels.GetSong())
		return cgi.Rp(ck, rp)
	case "changeGroup":
		rp := map[string]json.T{}
		group := cgi.RqString(mrq, "group")
		ok := false
		for _, g := range songs.GetGroups() {
			if group == g {
				sels.SetSongsGroup(group)
				ok = true
				break
			}
		}
		rp["ok"] = json.Wb(ok)
		return cgi.Rp(ck, rp)
	case "setLevel":
		songs.SetLevel(
			cgi.RqString(mrq, "group"),
			cgi.RqString(mrq, "song"),
			cgi.RqInt(mrq, "level"),
		)
		return cgi.RpEmpty(ck)
	case "setSel":
		rp := map[string]json.T{}
		group := cgi.RqString(mrq, "group")
		song := cgi.RqString(mrq, "song")
		ok := false
		for _, g := range songs.GetGroups() {
			if group == g {
				for _, sg := range songs.Read(g) {
					if sg.Id() == song {
						sels.SetSongGroup(group)
						sels.SetSong(song)
						ok = true
						break
					}
				}
				break
			}
		}
		rp["ok"] = json.Wb(ok)
		return cgi.Rp(ck, rp)
	case "setLapse":
		songs.SetLapse(
			cgi.RqString(mrq, "group"),
			cgi.RqString(mrq, "song"),
			cgi.RqDouble(mrq, "lapse"),
		)
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
