// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base initialization
package db

import (
	"github.com/dedeme/Bet1x2/data/cts"
	"github.com/dedeme/Bet1x2/db/year"
	"github.com/dedeme/Bet1x2/db/allClubs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/date"
	"path"
)

// Initialize data base.
func Initialize() {
	p := path.Join(cgi.Home(), cts.DataPath)
	version := path.Join(p, "version.txt")
	if !file.Exists(p) {
		file.Mkdir(p)
    allClubs.Mk(p)
    year.Mk(path.Join(p, date.Now().Format("%Y")))
		file.WriteAll(version, cts.DataVersion)
	}
	dbVersion := file.ReadAll(version)
	if dbVersion != cts.DataVersion {
		panic("Application can not continue.\n" +
			"Expected data version:\n" +
			cts.DataVersion +
			"\nBut found:\n" +
			dbVersion)
	}
}
