// Copyright 12-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base entry
package db

import (
	"github.com/dedeme/MMarket/data/cts"
	/*
		"github.com/dedeme/MMarket/db/acc/diariesDb"
		"github.com/dedeme/MMarket/db/acc/profitsDb"
		"github.com/dedeme/MMarket/db/calendarTb"
		"github.com/dedeme/MMarket/db/conf"
		"github.com/dedeme/MMarket/db/dailyChartsTb"
		"github.com/dedeme/MMarket/db/dailyTb"
		"github.com/dedeme/MMarket/db/fleas/flog"
		"github.com/dedeme/MMarket/db/fleas/fmodelsDb"
		"github.com/dedeme/MMarket/db/fleas/rankingTb"
	*/
	"github.com/dedeme/MMarket/db/log"
	/*
		"github.com/dedeme/MMarket/db/managersTb"
		"github.com/dedeme/MMarket/db/nicksTb"
		"github.com/dedeme/MMarket/db/performanceTb"
		"github.com/dedeme/MMarket/db/quotesDb"
		"github.com/dedeme/MMarket/db/refsDb"
		"github.com/dedeme/MMarket/db/sboxTb"
		"github.com/dedeme/MMarket/db/serversTb"
	*/
	"github.com/dedeme/MMarket/global/sync"
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
	dbVersion := file.ReadAll(version)
	if dbVersion != cts.DataVersion {
		panic("Application can not continue.\n" +
			"Expected data version:\n" +
			cts.DataVersion +
			"\nBut found:\n" +
			dbVersion)
	}

	log.Initialize(lk, p)
	/*	conf.Initialize(lk, p)
		calendarTb.Initialize(lk, p)
		nicksTb.Initialize(lk, p)
		quotesDb.Initialize(lk, p)
		refsDb.Initialize(lk, p)
		serversTb.Initialize(lk, p)
		managersTb.Initialize(lk, p)
		performanceTb.Initialize(lk, p)
		diariesDb.Initialize(lk, path.Join(p, "acc"))
		profitsDb.Initialize(lk, path.Join(p, "acc"))
		flog.Initialize(path.Join(p, "fleas"))
		fmodelsDb.Initialize(lk, path.Join(p, "fleas"))
		rankingTb.Initialize(lk, path.Join(p, "ranking"))
		sboxTb.Initialize(lk, path.Join(p, "daily"))
		dailyTb.Initialize(lk, path.Join(p, "daily"))
		dailyChartsTb.Initialize(lk, path.Join(p, "daily"))
	*/
}
