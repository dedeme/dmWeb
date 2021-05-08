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
		selections["pict"] = json.Ws("")
		selections["pictsPage"] = json.Wi(0)
		selections["song"] = json.Ws("")
		selections["lapse"] = json.Wd(0.0)
		file.WriteAll(fpath, json.Wo(selections).String())
	}
}

func read() map[string]json.T {
	return json.FromString(file.ReadAll(fpath)).Ro()
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

// Returns the current time lapse of reproduction of selected song.
func GetLapse() float64 {
	return read()["lapse"].Rd()
}

func write(key string, value json.T) {
	selections := read()
	selections[key] = value
	file.WriteAll(fpath, json.Wo(selections).String())
}

// Set the selected picture.
func SetPict(pict string) {
	write("pict", json.Ws(pict))
}

// Set the date of selected picture.
func SetPictDate(date string) {
	write("pictDate", json.Ws(date))
}

// Set the selected pictures page.
func SetPictsPage(page int) {
	write("pictsPage", json.Wi(page))
}

// Set the selected song (for relaxing).
func SetSong(song string) {
	write("song", json.Ws(song))
}

// Set the current time lapse of reproduction of selected song.
func SetLapse(lapse float64) {
	write("lapse", json.Wd(lapse))
}
