// Copyright 29-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Selections data base.
package sels

import (
	"fmt"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

const (
	fGroup      = "group"
	fPict       = "pict"
	fPictDate   = "pictDate"
	fPictsGroup = "pictsGroup"
	fPictsPage  = "pictsPage"

	fSongGroup  = "songGroup"
	fSong       = "song"
	fSongsGroup = "songsGroup"
	fSongsSong  = "songsSong"

	fDanceManagementGroup = "danceManagementGroup"
	fDanceSelectorGroup   = "danceSelectorGroup"

	pictTime       = "pictTime"
	shortDanceTime = "shortDanceTime"
	longDanceTime  = "longDanceTime"
)

var fpath string

// Initialize data base
func Initialize(parentDir string) {
	fpath = path.Join(parentDir, "sels.db")

	if !file.Exists(fpath) {
		selections := map[string]json.T{}
		file.WriteAll(fpath, json.Wo(selections).String())
	}

}

func read() map[string]json.T {
	return json.FromString(file.ReadAll(fpath)).Ro()
}

// Returns the selected group.
// Default "0"
func GetGroup() string {
	if groupJs, ok := read()[fGroup]; ok {
		return groupJs.Rs()
	}
	return "0"
}

// Returns the selected picture.
// Default ""
func GetPict() string {
	if pictJs, ok := read()[fPict]; ok {
		return pictJs.Rs()
	}
	return ""
}

// Returns the date of selected picture.
// Default current date-time.
func GetPictDate() string {
	if pictDateJs, ok := read()[fPictDate]; ok {
		return pictDateJs.Rs()
	}
	now := date.Now()
	return now.String() +
		fmt.Sprintf("%v:%v", now.Hour(), now.Minute()/GetPictTime())
}

// Returns the selected pictures group.
// Default "0"
func GetPictsGroup() string {
	if groupJs, ok := read()[fPictsGroup]; ok {
		return groupJs.Rs()
	}
	return "0"
}

// Returns the selected pictures page.
// Default 0
func GetPictsPage() int {
	if pageJs, ok := read()[fPictsPage]; ok {
		return pageJs.Ri()
	}
	return 0
}

// Returns the songs selected group
// Default ""
func GetSongGroup() string {
	if groupJs, ok := read()[fSongGroup]; ok {
		return groupJs.Rs()
	}
	return ""
}

// Returns the selected song (for relaxing).
// Default ""
func GetSong() string {
	if songJs, ok := read()[fSong]; ok {
		return songJs.Rs()
	}
	return ""
}

// Returns the selected group in songs management.
// Default ""
func GetSongsGroup() string {
	if groupJs, ok := read()[fSongsGroup]; ok {
		return groupJs.Rs()
	}
	return ""
}

// Returns the selected song for songs management.
// Default ""
func GetSongsSong() string {
	if songJs, ok := read()[fSongsSong]; ok {
		return songJs.Rs()
	}
	return ""
}

// Returns the dance management selected group
// Default ""
func GetDanceManagementGroup() string {
	if groupJs, ok := read()[fDanceManagementGroup]; ok {
		return groupJs.Rs()
	}
	return ""
}

// Returns the dance selector selected group
// Default ""
func GetDanceSelectorGroup() string {
	if groupJs, ok := read()[fDanceSelectorGroup]; ok {
		return groupJs.Rs()
	}
	return ""
}

// Returns time to show a picture (minutes).
// Default 2
func GetPictTime() int {
	if valueJs, ok := read()[pictTime]; ok {
		return valueJs.Ri()
	}
	return 2
}

// Returns duration time of short dance (minutes).
// Default 15
func GetShortDanceTime() int {
	if valueJs, ok := read()[shortDanceTime]; ok {
		return valueJs.Ri()
	}
	return 15
}

// Returns duration time of long dance (minutes).
// Default 45
func GetLongDanceTime() int {
	if valueJs, ok := read()[longDanceTime]; ok {
		return valueJs.Ri()
	}
	return 45
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

func write(key string, value json.T) {
	selections := read()
	selections[key] = value
	file.WriteAll(fpath, json.Wo(selections).String())
}

// Set the selected group.
func SetGroup(group string) {
	write(fGroup, json.Ws(group))
}

// Set the selected picture.
func SetPict(pict string) {
	write(fPict, json.Ws(pict))
}

// Set the date of selected picture.
func SetPictDate(date string) {
	write(fPictDate, json.Ws(date))
}

// Set the selected pictures group.
func SetPictsGroup(group string) {
	write(fPictsGroup, json.Ws(group))
}

// Set the selected pictures page.
func SetPictsPage(page int) {
	write(fPictsPage, json.Wi(page))
}

// Set the selected song group (for relaxing).
func SetSongGroup(group string) {
	write(fSongGroup, json.Ws(group))
}

// Set the selected song (for relaxing).
func SetSong(song string) {
	write(fSong, json.Ws(song))
}

// Set the selected group for songs management.
func SetSongsGroup(group string) {
	write(fSongsGroup, json.Ws(group))
}

// Set the selected song for songs management.
func SetSongsSong(song string) {
	write(fSongsSong, json.Ws(song))
}

// Set the dance management selected group
func SetDanceManagementGroup(group string) {
	write(fDanceManagementGroup, json.Ws(group))
}

// Set the dance selector selected group
func SetDanceSelectorGroup(group string) {
	write(fDanceSelectorGroup, json.Ws(group))
}

// Set the time to show a picture (minutes).
func SetPictTime(value int) {
	write(pictTime, json.Wi(value))
}

// Set the time to show a picture (minutes).
func SetShortDanceTime(value int) {
	write(shortDanceTime, json.Wi(value))
}

// Set the time to show a picture (minutes).
func SetLongDanceTime(value int) {
	write(longDanceTime, json.Wi(value))
}
