// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base initialization
package db

import (
	"github.com/dedeme/CashFlow/data/cts"
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/file"
	"path"
)

// Initialize data base.
func Initialize() {
	p := path.Join(cgi.Home(), cts.DataPath)
	version := path.Join(p, "version.txt")
	if !file.Exists(p) {
		file.Mkdir(p)
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

	years.Initialize(p)
}
