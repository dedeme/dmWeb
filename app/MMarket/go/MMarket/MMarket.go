// Copyright 12-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main file
package main

import (
	"fmt"
	"github.com/dedeme/MMarket/data/activity"
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/db"
	"github.com/dedeme/MMarket/db/conf"
	"github.com/dedeme/MMarket/db/log"
	"github.com/dedeme/MMarket/global/sync"

	"github.com/dedeme/MMarket/scheduler"
	/*	"github.com/dedeme/MMarket/server"
		"github.com/dedeme/MMarket/tests"
	*/
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/sys"
	"net"
	"os"
)

// Help messsage.
func help() {
	fmt.Println("Usage: MMarket [start | stop | test | help]")
}

// Main function.
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
			log.Info(lk, "MMarket started")
			act = scheduler.InitialActivity(conf.Activity(lk))
		})

		svCh := make(chan int)
		//go server.Start(svCh)

		schCh := make(chan int)
		//go scheduler.Start(schCh, act)

		<-svCh
		<-schCh

		sync.Run(func(lk sync.T) { log.Info(lk, "MMarket stopped") })
	case "stop":
		conn, err := net.Dial("tcp4", "127.0.0.1:"+cts.Port)
		if err != nil {
			fmt.Println("Fail connecting to stop MMarket")
			return
		}
		_, err = conn.Write([]byte("end"))
		conn.Close()
		if err != nil {
			fmt.Println("Fail stopping MMarket")
		}
	default:
		help()
	}
	fmt.Println("here")
}
