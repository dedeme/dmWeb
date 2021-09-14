// Copyright 25-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Years data base initialization
package years

import (
	"github.com/dedeme/CashFlow/db/years/year"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"path"
	"strconv"
)

var dpath string

// Initialize data base.
func Initialize(parentDir string) {
	dpath = path.Join(parentDir, "years")
	if !file.IsDirectory(dpath) {
		file.Mkdir(dpath)
	}
	y := date.Now().Year()
	ypath := YearPath(strconv.Itoa(y))

	if !file.IsDirectory(ypath) {
		year.Mk(ypath)
	}

	ypath1 := YearPath(strconv.Itoa(y + 1))
	if !file.IsDirectory(ypath1) {
		year.MkFrom(ypath, ypath1)
	}
}

// Returns data base path of a year.
func YearPath(y string) string {
	return path.Join(dpath, y)
}

// Returns the list of years
func List() []string {
	var r []string
	for _, fi := range file.List(dpath) {
		r = append(r, fi.Name())
	}
	return r
}
