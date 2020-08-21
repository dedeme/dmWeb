// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base entry
package db

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/db/calendarTb"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/sys"
	"path"
)

// Initialize data base.
func Initialize() {
	p := path.Join(sys.Home(), cts.DataPath)
	version := path.Join(p, "version.txt")
	if !file.Exists(p) {
		file.Mkdir(p)
		file.Mkdir(path.Join(p, "fleas"))
		file.Mkdir(path.Join(p, "ranking"))
		file.Mkdir(path.Join(p, "acc"))
		file.Mkdir(path.Join(p, "daily"))
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
	calendarTb.Initialize(p)
	nicksTb.Initialize(p)
}
