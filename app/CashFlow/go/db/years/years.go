// Copyright 25-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Years data base initialization
package years

import (
	"github.com/dedeme/CashFlow/db/years/year"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/time"
)

var dpath string

// Initialize data base.
func Initialize(parentDir string) {
	dpath = path.Cat(parentDir, "years")
	if !file.IsDirectory(dpath) {
		file.Mkdir(dpath)
	}
	y := time.Year(time.Now())
	ypath := YearPath(str.Fmt("%v", y))

	if !file.IsDirectory(ypath) {
		year.Mk(ypath)
	}

	ypath1 := YearPath(str.Fmt("%v", y+1))
	if !file.IsDirectory(ypath1) {
		year.MkFrom(ypath, ypath1)
	}
}

// Returns data base path of a year.
func YearPath(y string) string {
	return path.Cat(dpath, y)
}

// Returns the list of years
func List() []string {
	var r []string
	for _, fname := range file.Dir(dpath) {
		r = append(r, fname)
	}
	return r
}
