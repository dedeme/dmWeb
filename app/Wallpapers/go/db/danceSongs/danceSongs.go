// Copyright 17-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Dance songs data base.
package danceSongs

import (
	"github.com/dedeme/Wallpapers/data/danceSong"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"io/ioutil"
	"path"
	"strings"
)

var dir string

func fpath(group string) string {
	return path.Join(dir, "dance_"+group+".db")
}

// Initialize data base
func Initialize(parentDir string) {
	dir = parentDir
}

// Returns groups list
func GetGroups() []string {
	var groups []string
	infs, err := ioutil.ReadDir("/dm/danceBic")
	if err != nil {
		panic(err)
	}

	for _, inf := range infs {
		if inf.IsDir() {
			groups = append(groups, inf.Name())
		}
	}

	if len(groups) == 0 {
		panic("Dance groups not found")
	}

	return groups
}

func update(group string) {
	p := fpath(group)
	if !file.Exists(p) {
		Write(group, []*danceSong.T{})
	}

	var oldSongs []*danceSong.T
	var newSongs []*danceSong.T
	for _, songJs := range json.FromString(file.ReadAll(p)).Ra() {
		oldSongs = append(oldSongs, danceSong.FromJs(songJs))
	}
	for _, songId := range readSongList(group) {
		ok := false
		for _, song := range oldSongs {
			if song.Id() == songId {
				newSongs = append(newSongs, song)
				ok = true
				break
			}
		}
		if !ok {
			newSongs = append(newSongs, danceSong.New(songId))
		}
	}

	Write(group, newSongs)
}

func readSongList(group string) []string {
	var songs []string
	infs, err := ioutil.ReadDir(path.Join("/dm/danceBic", group))
	if err != nil {
		panic(err)
	}

	for _, inf := range infs {
		if strings.HasSuffix(inf.Name(), ".mp3") {
			songs = append(songs, inf.Name())
		}
	}

	if len(songs) == 0 {
		panic("Songs not found in group " + dir)
	}

	return songs
}

// Returns songs of a group.
func ReadJs(group string) json.T {
	update(group)
	return json.FromString(file.ReadAll(fpath(group)))
}

// Returns song groups.
func Read(group string) []*danceSong.T {
	var songs []*danceSong.T
	for _, songJs := range ReadJs(group).Ra() {
		songs = append(songs, danceSong.FromJs(songJs))
	}
	return songs
}

// Write song Groups
func Write(group string, songs []*danceSong.T) {
	var songsJs []json.T
	for _, song := range songs {
		songsJs = append(songsJs, song.ToJs())
	}

	file.WriteAll(fpath(group), json.Wa(songsJs).String())
}

// Set level of song 'group'-'id'.
func SetLevel(group string, id string, level int) {
	songs := danceSong.SetLevel(Read(group), id, level)
	Write(group, songs)
}

// Set speed of song 'group'-'id'.
func SetSpeed(group string, id string, speed int) {
	songs := danceSong.SetSpeed(Read(group), id, speed)
	Write(group, songs)
}
