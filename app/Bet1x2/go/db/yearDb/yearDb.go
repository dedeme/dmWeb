// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Year data base initialization
package yearDb

import (
	"github.com/dedeme/Bet1x2/data/cts"
	"github.com/dedeme/Bet1x2/db/yearDb/clubs"
	"github.com/dedeme/Bet1x2/db/yearDb/matchdays"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/file"
	"path"
)

func dpath(year string) string {
	return path.Join(cgi.Home(), cts.DataPath, year)
}

// Create year data base.
func Mk(year string) {
	file.Mkdir(dpath(year))
	clubs.Mk(year)
	matchdays.Mk(year)
}

// Create year data base if it does not exist.
func MkIfNot(year string) {
	if !file.IsDirectory(dpath(year)) {
		file.Mkdir(dpath(year))
		clubs.Mk(year)
		matchdays.Mk(year)
	}
}

// Returns the year list.
func List() []string {
	var r []string
	for _, fi := range file.List(path.Join(cts.Home, cts.DataPath)) {
		if fi.IsDir() {
			r = append(r, fi.Name())
		}
	}
	return r
}
