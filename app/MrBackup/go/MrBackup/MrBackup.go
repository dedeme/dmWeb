// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main file
package main

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/db"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/MrBackup/scheduler"
	"github.com/dedeme/MrBackup/server"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/file"
	"net"
	"os"
)

func help() {
	fmt.Println("Usage: MrBackup [start | stop | help]")
}

func main() {
	cgi.Initialize(file.Home(), cts.Expiration)

	db.Initialize()

	if len(os.Args) != 2 {
		help()
		return
	}
	switch os.Args[1] {

	case "start":
		log.Info("MrBackup started")

		svCh := make(chan int)
		go server.Start(svCh)

		schCh := make(chan int)
		go scheduler.Start(schCh)

		<-svCh
		<-schCh

		log.Info("MrBackup stopped")
	case "stop":
		conn, err := net.Dial("tcp4", "127.0.0.1:"+cts.Port)
		if err != nil {
			fmt.Println("Fail connecting to stop MrBackup")
			return
		}
		_, err = conn.Write([]byte("end"))
		conn.Close()
		if err != nil {
			fmt.Println("Fail stopping MrMarket")
		}
	default:
		help()
	}
}
