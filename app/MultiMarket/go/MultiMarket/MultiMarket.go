// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main file
package main

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/activity"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/db"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/MultiMarket/scheduler"
	"github.com/dedeme/MultiMarket/server"
	"github.com/dedeme/MultiMarket/tests"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/sys"
	"net"
	"os"
)

func help() {
	fmt.Println("Usage: MultiMarket [start | stop | test | help]")
}

func main() {
	sys.Initialize(cts.AppName)
	cgi.Initialize(sys.Home(), cts.Expiration)

	sync.Run(func(lk sync.T) { db.Initialize(lk) })

	if len(os.Args) != 2 {
		help()
		return
	}
	switch os.Args[1] {
	case "start":
		var act *activity.T
		sync.Run(func(lk sync.T) {
			log.Info(lk, "MultiMarket started")
			act = scheduler.InitialActivity(conf.Activity(lk))
		})

		svCh := make(chan int)
		go server.Start(svCh)

		schCh := make(chan int)
		go scheduler.Start(schCh, act)

		<-svCh
		<-schCh

		sync.Run(func(lk sync.T) { log.Info(lk, "MultiMarket stopped") })
	case "stop":
		conn, err := net.Dial("tcp4", "127.0.0.1:"+cts.Port)
		if err != nil {
			fmt.Println("Fail connecting to stop MultiMarket")
			return
		}
		_, err = conn.Write([]byte("end"))
		conn.Close()
		if err != nil {
			fmt.Println("Fail stopping MultiMarket")
		}
	case "test":
		tests.Run()
	default:
		help()
	}
}
