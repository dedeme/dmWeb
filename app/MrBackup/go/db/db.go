// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global constants
package db

import (
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/db/conf"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/path"
)

func Initialize() {
	p := path.Cat(file.Home(), cts.Home, cts.DataPath)
	version := path.Cat(p, "version.txt")
	if !file.Exists(p) {
		file.Mkdir(p)
		file.Write(version, cts.DataVersion)
	}
	log.Initialize(p)
	dbVersion := file.Read(version)
	if dbVersion != cts.DataVersion {
		panic("Application can not continue.\n" +
			"Expected data version:\n" +
			cts.DataVersion +
			"\nBut found:\n" +
			dbVersion)
	}
	conf.Initialize(p)
}
