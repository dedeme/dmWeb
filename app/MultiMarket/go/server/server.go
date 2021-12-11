// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server process
package server

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/stopper"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/MultiMarket/server/mainHub"
	"io"
	"net"
	"os"
	"strings"
)

// Auxiliar function
func logError(msg string) {
	sync.Run(func(lk sync.T) {
		log.Error(lk, msg)
	})
}

func Start(ch chan int) {
	sv, err := net.Listen("tcp4", "127.0.0.1:"+cts.Port)
	if err != nil {
		logError(fmt.Sprintf(
			"Server listenning port %s failed (%s)", cts.Port, err,
		))
		fmt.Printf(
			"Server listenning port %s failed (%s)", cts.Port, err,
		)
		os.Exit(1)
	}
	for !stopper.Stop {
		client, err := sv.Accept()
		if err != nil {
			logError("Server accepting client failed")
			fmt.Printf("Server accepting client failed")
			continue
		}

		var rq strings.Builder
		buf := make([]byte, 65536)
		for {
			n, err := client.Read(buf)
			rq.Write(buf[:n])

			if err != nil {
				if err != io.EOF {
					logError(fmt.Sprintf(
						"Server reading client failed.\nError: %v\nCurrent input:\n%s",
						err, rq.String(),
					))
					fmt.Printf(
						"Server listenning port %s failed (%s)", cts.Port, err,
					)
				}
				break
			}
		}

		rqs := rq.String()
		if rqs == "end" {
			stopper.Stop = true
			client.Close()
			continue
		}

		rp := mainHub.Process(rqs)
		_, err = client.Write([]byte(rp))
		client.Close()

		if err != nil {
			logError(fmt.Sprintf("Server fail writing to client.\nError:\n%v", err))
		}
	}
	sv.Close()
	ch <- 0
}
