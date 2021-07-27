// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base entry
package db

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/db/acc/diariesDb"
	"github.com/dedeme/MultiMarket/db/acc/profitsDb"
	"github.com/dedeme/MultiMarket/db/calendarTb"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/db/dailyChartsTb"
	"github.com/dedeme/MultiMarket/db/dailyTb"
	"github.com/dedeme/MultiMarket/db/fleas/fmodelsDb"
	"github.com/dedeme/MultiMarket/db/fleas/rangesTb"
	"github.com/dedeme/MultiMarket/db/fleas/resultsDb"
	"github.com/dedeme/MultiMarket/db/investorsTb"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/performanceTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/db/refsDb"
	"github.com/dedeme/MultiMarket/db/sboxTb"
	"github.com/dedeme/MultiMarket/db/serversTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/sys"
	"path"
)

// Initialize data base.
func Initialize(lk sync.T) {
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
	log.Initialize(lk, p)
	dbVersion := file.ReadAll(version)
	if dbVersion != cts.DataVersion {
		panic("Application can not continue.\n" +
			"Expected data version:\n" +
			cts.DataVersion +
			"\nBut found:\n" +
			dbVersion)
	}

	conf.Initialize(lk, p)
	calendarTb.Initialize(lk, p)
	nicksTb.Initialize(lk, p)
	quotesDb.Initialize(lk, p)
	refsDb.Initialize(lk, p)
	serversTb.Initialize(lk, p)
	investorsTb.Initialize(lk, p)
	performanceTb.Initialize(lk, p)
	diariesDb.Initialize(lk, path.Join(p, "acc"))
	profitsDb.Initialize(lk, path.Join(p, "acc"))
	fmodelsDb.Initialize(lk, path.Join(p, "fleas"))
	rangesTb.Initialize(lk, path.Join(p, "fleas"))
	sboxTb.Initialize(lk, path.Join(p, "daily"))
	dailyTb.Initialize(lk, path.Join(p, "daily"))
	dailyChartsTb.Initialize(lk, path.Join(p, "daily"))
	resultsDb.Initialize(lk, path.Join(p, "results"))
}
