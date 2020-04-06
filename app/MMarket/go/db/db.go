// Copyright 04-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Intializer of data base
package db

import (
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/db/calendarDb"
	"github.com/dedeme/MMarket/db/conf"
	"github.com/dedeme/MMarket/db/log"
	"github.com/dedeme/golib/file"
	liblog "github.com/dedeme/golib/log"
	"github.com/dedeme/golib/sys"
	"path"
)

var ch chan func()

// Initializes data base
func Initialize(channel chan func()) {
	ch = channel
	p := path.Join(sys.Home(), cts.DATA_PATH)
	version := path.Join(p, "version.txt")
	if !file.Exists(p) {
		file.Mkdirs(p)
		file.WriteAll(version, cts.DATA_VERSION)
	} else {
		dbVersion := file.ReadAll(version)
		if dbVersion != cts.DATA_VERSION {
			liblog.Fatal("Application can not continue.\n" +
				"Expected:\n" + cts.DATA_VERSION + "But found:\n" + dbVersion)
		}
	}

	log.Initialize(p)
	conf.Initialize(p)
	calendarDb.Initialize(p)
}

// Synchronizates 'fn'
func Sync(fn func()) {
	ch <- fn
	<-ch
}
