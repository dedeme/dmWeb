// Copyright 04-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main file
package main

import (
	"fmt"
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/db"
	"github.com/dedeme/MMarket/db/log"
	"github.com/dedeme/MMarket/scheduler"
	"github.com/dedeme/MMarket/server"
	"github.com/dedeme/golib/sys"
	"net/http"
	"os"
	"strings"
	//"io/ioutil"
)

func run(ac chan func()) {
	for {
		fn := <-ac
		if fn == nil {
			log.Info("Actor stoped")
			break
		}
		if !server.Stoped() {
			fn()
		}
		ac <- nil
	}
	ac <- nil
}

func test() bool {
	msg := strings.NewReader("test")
	rp, _ := http.Post("http://localhost:"+cts.PORT, "text/plain", msg)
	return rp != nil
}

func end() {
	msg := strings.NewReader("end")
	http.Post("http://localhost:"+cts.PORT, "text/plain", msg)
}

func help() {
	fmt.Println("Usage: MMarket [start | stop | test | help]")
}

func initialize(ac chan func()) {
	sys.Initialize(cts.APP_NAME)
	db.Initialize(ac)
}

func main() {
	actor := make(chan func())
	initialize(actor)

	if len(os.Args) != 2 {
		help()
	} else {
		switch os.Args[1] {
		case "start":
			if test() {
				fmt.Println("MMarket is active")
				return
			}
			log.Info("MMarket started")

			go run(actor)
			finalizer := make(chan bool)

			go scheduler.Run(actor, finalizer)
			server.Run(actor)
			<-finalizer

			actor <- nil
			<-actor
			log.Info("MMarket stoped")
		case "test":
			if test() {
				fmt.Println("MMarket is active")
			} else {
				fmt.Println("MMarket is stopped")
			}
		case "end":
			end()
		default:
			help()
		}
	}
}
