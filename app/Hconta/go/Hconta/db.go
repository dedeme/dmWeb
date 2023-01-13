// Copyright 19-06-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base
package main

import (
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/time"
)

/// Reads TimeStamp and All-Data.
func readData() (
	timeStamp string, jyear string, jyears string, lang string, data string) {
	dir := path.Cat(HOME, "data")
	if !file.Exists(dir) {
		file.Mkdir(dir)
		dt := time.Now()
		year := time.Fmt("%Y", dt)
		file.Write(
			path.Cat(dir, "control.tb"),
			js.Wa([]string{js.Ws(""), js.Ws(year), js.Ws("es")}),
		)
		file.Write(path.Cat(dir, year+".db"), js.Wa([]string{}))
	}

	a := js.Ra(file.Read(path.Cat(dir, "control.tb")))
	year := js.Rs(a[1])
	var years []string
	for _, fname := range file.Dir(dir) {
		if str.Ends(fname, ".db") {
			years = append(years, fname[0:len(fname)-3])
		}
	}
	arr.Sort(years, func(y1 string, y2 string) bool {
		return y1 < y2
	})
	ok := false
	for _, y := range years {
		if y == year {
			ok = true
		}
	}
	if !ok {
		year = years[len(years)-1]
	}

	var ys []string
	for _, y := range years {
		ys = append(ys, js.Ws(y))
	}

	timeStamp = a[0]
	lang = a[2]
	jyear = js.Ws(year)
	jyears = js.Wa(ys)
	data = file.Read(path.Cat(dir, year+".db"))
	return
}

/// Writes 'data' and updates timeStamp. Returns the new timeStamp.
/// If 'timeStamp' is out of date, returns json.Ws("").
func writeData(
	timeStamp string, year string, lang string, data string,
) string {
	dir := path.Cat(HOME, "data")
	a := js.Ra(file.Read(path.Cat(dir, "control.tb")))
	oldTimeStamp := a[0]
	if oldTimeStamp != timeStamp {
		return js.Ws("")
	}
	y := js.Rs(year)
	timeStamp = js.Ws(time.Fmt("%Y%M%D%T", time.Now()))
	file.Write(
		path.Cat(dir, "control.tb"),
		js.Wa([]string{timeStamp, js.Ws(y), lang}),
	)
	file.Write(path.Cat(dir, y+".db"), data)
	return timeStamp
}

/// Change current year.
func writeYear(timeStamp string, year string) string {
	dir := path.Cat(HOME, "data")
	a := js.Ra(file.Read(path.Cat(dir, "control.tb")))
	oldTimeStamp := a[0]
	if oldTimeStamp != timeStamp {
		return js.Ws("")
	}
	timeStamp = js.Ws(time.Fmt("%Y%M%D%T", time.Now()))
	file.Write(
		path.Cat(dir, "control.tb"),
		js.Wa([]string{timeStamp, year, a[2]}),
	)
	return timeStamp
}
