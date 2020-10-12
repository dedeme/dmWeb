// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global constants
package db

import (
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/db/conf"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/sys"
	"path"
)

func Initialize() {
	p := path.Join(sys.Home(), cts.DataPath)
	version := path.Join(p, "version.txt")
	if !file.Exists(p) {
		file.Mkdir(p)
		file.WriteAll(version, cts.DataVersion)
	}
	log.Initialize(p)
	dbVersion := file.ReadAll(version)
	if dbVersion != cts.DataVersion {
		panic("Application can not continue.\n" +
			"Expected data version:\n" +
			cts.DataVersion +
			"\nBut found:\n" +
			dbVersion)
	}
	conf.Initialize(p)
}
