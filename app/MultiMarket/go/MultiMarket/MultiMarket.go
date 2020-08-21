// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main file
package main

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/db"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/MultiMarket/server"
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
	db.Initialize()

	if len(os.Args) != 2 {
		help()
		return
	}
	switch os.Args[1] {
	case "start":
		sync.Run(func(lk sync.T) { log.Info(lk, "MultiMarket started") })

		svCh := make(chan int)
		go server.Start(svCh)

		<-svCh

		fmt.Println("ok")
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
