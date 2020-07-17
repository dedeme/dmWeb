// Copyright 19-06-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base
package main

import (
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
	"sort"
	"strings"
)

/// Reads TimeStamp and All-Data.
func readData() (
  timeStamp json.T, jyear json.T, jyears json.T, lang json.T, data json.T) {
	dir := path.Join(HOME, "data")
	if !file.Exists(dir) {
		file.Mkdir(dir)
		dt := date.Now()
		year := dt.Format("%Y")
		file.WriteAll(
			path.Join(dir, "control.tb"),
			json.Wa([]json.T{json.Ws(""), json.Ws(year), json.Ws("es")}).String(),
		)
		file.WriteAll(path.Join(dir, year+".db"), json.Wa([]json.T{}).String())
	}

	a := json.FromString(file.ReadAll(path.Join(dir, "control.tb"))).Ra()
	year := a[1].Rs()
	var years []string
	for _, fs := range file.List(dir) {
		if strings.HasSuffix(fs.Name(), ".db") {
			years = append(years, fs.Name()[0:len(fs.Name())-3])
		}
	}
	sort.Strings(years)
	ok := false
	for _, y := range years {
		if y == year {
			ok = true
		}
	}
	if !ok {
		year = years[len(years)-1]
	}

	var ys []json.T
	for _, y := range years {
		ys = append(ys, json.Ws(y))
	}

	timeStamp = a[0]
  lang = a[2]
	jyear = json.Ws(year)
	jyears = json.Wa(ys)
	data = json.FromString(file.ReadAll(path.Join(dir, year+".db")))
	return
}

/// Writes 'data' and updates timeStamp. Returns the new timeStamp.
/// If 'timeStamp' is out of date, returns json.Ws("").
func writeData(
  timeStamp json.T, year json.T, lang json.T, data json.T,
) json.T {
	dir := path.Join(HOME, "data")
	a := json.FromString(file.ReadAll(path.Join(dir, "control.tb"))).Ra()
	oldTimeStamp := a[0]
	if oldTimeStamp.String() != timeStamp.String() {
		return json.Ws("")
	}
	y := year.Rs()
	timeStamp = json.Ws(date.Now().Format("%Y%M%D%T"))
	file.WriteAll(
		path.Join(dir, "control.tb"),
		json.Wa([]json.T{timeStamp, json.Ws(y), lang}).String(),
	)
	file.WriteAll(path.Join(dir, y+".db"), data.String())
	return timeStamp
}

/// Change current year.
func writeYear(timeStamp json.T, year json.T) json.T {
	dir := path.Join(HOME, "data")
	a := json.FromString(file.ReadAll(path.Join(dir, "control.tb"))).Ra()
	oldTimeStamp := a[0]
	if oldTimeStamp.String() != timeStamp.String() {
		return json.Ws("")
	}
	timeStamp = json.Ws(date.Now().Format("%Y%M%D%T"))
	file.WriteAll(
		path.Join(dir, "control.tb"),
		json.Wa([]json.T{timeStamp, year, a[2]}).String(),
	)
	return timeStamp
}

