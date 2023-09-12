// Copyright 07-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Songs data base.
package songs

import (
	"github.com/dedeme/Wallpapers/data/sighter"
	"github.com/dedeme/Wallpapers/data/song"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"io/ioutil"
	"path"
	"strings"
)

var dir string // Directory of original songs

func fpath(group string) string {
	return path.Join(dir, "songs_"+group+".db")
}

// Initialize data base
func Initialize(parentDir string) {
	dir = parentDir
}

// Returns groups list
func GetGroups() []string {
	var groups []string
	infs, err := ioutil.ReadDir("/var/www/html/relax")
	if err != nil {
		panic(err)
	}

	for _, inf := range infs {
		if inf.IsDir() {
			groups = append(groups, inf.Name())
		}
	}

	if len(groups) == 0 {
		panic("Song groups not found")
	}

	return groups
}

func update(group string) {
	p := fpath(group)
	if !file.Exists(p) {
		Write(group, []*song.T{})
	}

	var oldSongs []*song.T
	var newSongs []*song.T
	for _, songJs := range json.FromString(file.ReadAll(p)).Ra() {
		oldSongs = append(oldSongs, song.FromJs(songJs))
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
			newSongs = append(newSongs, song.New(songId))
		}
	}

	Write(group, newSongs)
}

func readSongList(group string) []string {
	var songs []string
	infs, err := ioutil.ReadDir(path.Join("/var/www/html/relax", group))
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
func Read(group string) []*song.T {
	var songs []*song.T
	for _, songJs := range ReadJs(group).Ra() {
		songs = append(songs, song.FromJs(songJs))
	}
	return songs
}

// Write song Groups
func Write(group string, songs []*song.T) {
	var songsJs []json.T
	for _, song := range songs {
		songsJs = append(songsJs, song.ToJs())
	}

	file.WriteAll(fpath(group), json.Wa(songsJs).String())
}

// Set level of song 'group'-'id'.
func SetLevel(group string, id string, level int) {
	songs := song.SetLevel(Read(group), id, level)
	Write(group, songs)
}

// Set level of song 'group'-'id'.
func SetLapse(group string, id string, lapse float64) {
	songs := song.SetLapse(Read(group), id, lapse)
	Write(group, songs)
}

// Returns the next picture
func Next() (group string, s *song.T) {
	gr, sg := sighter.Next(
		GetGroups,
		func(group string) []sighter.T {
			return song.ToSighters(Read(group))
		},
		func(group string, ss []sighter.T) {
			Write(group, song.FromSighters(ss))
		},
	)
	group = gr
	s = sg.(*song.T)
	return
}
