// Copyright 14-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main file
package main

import (
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/db"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/QMarket/scheduler"
	"github.com/dedeme/QMarket/server"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/sys"
	"net"
	"os"
)

func help() {
	fmt.Println("Usage: QMarket [start | stop | test | help]")
}

func main() {
	sys.Initialize(cts.AppName)
	cgi.Initialize(sys.Home(), cts.Expiration)
	lock.Initialize()
	db.Initialize()
	scheduler.Initialize()

	if len(os.Args) != 2 {
		help()
		return
	}
	switch os.Args[1] {
	case "start":
		logTb.Info("QMarket started")

		svCh := make(chan int)
		go server.Start(svCh)

		schCh := make(chan int)
		go scheduler.Start(schCh)

		<-svCh
		<-schCh

		lock.Run(func() {
			logTb.Info("QMarket stopped")
		})
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
	default:
		help()
	}
}
