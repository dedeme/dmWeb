// Copyright 29-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Selections data base.
package sels

import (
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initialize data base
func Initialize(parentDir string) {
	fpath = path.Join(parentDir, "sels.db")

	if !file.Exists(fpath) {
		selections := map[string]json.T{}
		selections["group"] = json.Ws("0")
		selections["pict"] = json.Ws("")
		selections["pictsGroup"] = json.Ws("0")
		selections["pictsPage"] = json.Wi(0)
    selections["danceManagementGroup"] = json.Ws("")
    selections["danceSelectorGroup"] = json.Ws("")
		file.WriteAll(fpath, json.Wo(selections).String())
	}
}

func read() map[string]json.T {
	return json.FromString(file.ReadAll(fpath)).Ro()
}

// Returns the selected group.
// If returns is "", no picture has been selected.
func GetGroup() string {
	return read()["group"].Rs()
}

// Returns the selected picture.
// If returns is "", no picture has been selected.
func GetPict() string {
	return read()["pict"].Rs()
}

// Returns the date of selected picture.
func GetPictDate() string {
	return read()["pictDate"].Rs()
}

// Returns the selected pictures group.
func GetPictsGroup() string {
	return read()["pictsGroup"].Rs()
}

// Returns the selected pictures page.
// Default 0.
func GetPictsPage() int {
	return read()["pictsPage"].Ri()
}

// Returns the selected song (for relaxing).
// If returns is "", no song has been selected.
func GetSong() string {
	return read()["song"].Rs()
}

// Returns the total time of selected song.
func GetTime() int64 {
	return read()["time"].Rl()
}

// Reuturns the dance management selected group
func GetDanceManagementGroup (groups []string) string {
  r := read()["danceManagementGroup"].Rs()
  if r == "" {
    r = groups[0]
  }
  return r
}

// Reuturns the dance selector selected group
func GetDanceSelectorGroup (groups []string) string {
  r := read()["danceSelectorGroup"].Rs()
  if r == "" {
    r = groups[0]
  }
  return r
}

func write(key string, value json.T) {
	selections := read()
	selections[key] = value
	file.WriteAll(fpath, json.Wo(selections).String())
}

// Set the selected group.
func SetGroup(group string) {
	write("group", json.Ws(group))
}

// Set the selected picture.
func SetPict(pict string) {
	write("pict", json.Ws(pict))
}

// Set the date of selected picture.
func SetPictDate(date string) {
	write("pictDate", json.Ws(date))
}

// Set the selected pictures group.
func SetPictsGroup(group string) {
	write("pictsGroup", json.Ws(group))
}

// Set the selected pictures page.
func SetPictsPage(page int) {
	write("pictsPage", json.Wi(page))
}

// Set the selected song (for relaxing).
func SetSong(song string) {
	write("song", json.Ws(song))
}

// Set the the dance management selected group
func SetDanceManagementGroup(group string) {
  write("danceManagementGroup", json.Ws(group))
}

// Set the the dance selector selected group
func SetDanceSelectorGroup(group string) {
  write("danceSelectorGroup", json.Ws(group))
}

