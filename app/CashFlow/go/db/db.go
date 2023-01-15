// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base initialization
package db

import (
	"github.com/dedeme/CashFlow/data/cts"
	"github.com/dedeme/CashFlow/db/years"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/path"
)

// Initialize data base.
func Initialize() {
	p := path.Cat(cgi.Home(), cts.DataPath)
	version := path.Cat(p, "version.txt")
	if !file.Exists(p) {
		file.Mkdir(p)
		file.Write(version, cts.DataVersion)
	}
	dbVersion := file.Read(version)
	if dbVersion != cts.DataVersion {
		panic("Application can not continue.\n" +
			"Expected data version:\n" +
			cts.DataVersion +
			"\nBut found:\n" +
			dbVersion)
	}

	years.Initialize(p)
}
