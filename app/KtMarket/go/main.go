// Copyright 02-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main file
package main

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/sch"
	"github.com/dedeme/KtMarket/server"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/cryp"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/sys"
	"github.com/dedeme/ktlib/thread"
	"github.com/dedeme/ktlib/websv"
)

func help() {
	sys.Println("Usage: KtMarket [start | stop | test | help]")
}

func start() {
	thread.Run(func() {
		sch.Start()
	})
	server.Start()
	for {
		if !sch.Busy {
			break
		}
		sys.Sleep(500)
	}
}

func stop() {
	websv.Stop(cts.Port, cryp.Encode(cryp.Key(cts.AppName, 100), cryp.Key("end", 100)))
}

func test() {
	sch.HistoricUpdate()
	sys.Println("All tests ok")
}

func main() {
	if len(sys.Args()) != 2 {
		help()
		return
	}

	sys.Rand()
	cgi.Initialize(cts.HomePath, cts.Expiration)
	db.Initialize()
	log.Initialize(path.Cat(cts.DataPath, "log.tb"))

	switch sys.Args()[1] {
	case "start":
		start()
	case "stop":
		stop()
	case "test":
		test()
	default:
		help()
	}

	sys.Println("Program end.")
}
