// Copyright 14-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base initializer
package db

import (
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/db/acc/diariesDb"
	"github.com/dedeme/QMarket/db/acc/profitsDb"
	"github.com/dedeme/QMarket/db/calendarTb"
	"github.com/dedeme/QMarket/db/confTb"
	"github.com/dedeme/QMarket/db/dailyDb/dailyChartsTb"
	"github.com/dedeme/QMarket/db/dailyDb/dailyTb"
	"github.com/dedeme/QMarket/db/dailyDb/sboxTb"
	"github.com/dedeme/QMarket/db/investorsTb"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/db/modelsDb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/performanceTb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/db/ranksDb"
	"github.com/dedeme/QMarket/db/serversTb"
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
		file.Mkdir(path.Join(p, "acc"))
		file.Mkdir(path.Join(p, "daily"))
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

	logTb.Initialize(p)
	confTb.Initialize(p)
	calendarTb.Initialize(p)
	nicksTb.Initialize(p)
	quotesDb.Initialize(p)
	serversTb.Initialize(p)
	investorsTb.Initialize(p)
	modelsDb.Initialize(p)
	ranksDb.Initialize(p)

	diariesDb.Initialize(path.Join(p, "acc"))
	profitsDb.Initialize(path.Join(p, "acc"))
	sboxTb.Initialize(path.Join(p, "daily"))
	dailyTb.Initialize(path.Join(p, "daily"))
	dailyChartsTb.Initialize(path.Join(p, "daily"))

	performanceTb.Initialize(p)
}
