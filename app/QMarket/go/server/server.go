// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server process
package server

import (
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/QMarket/server/mainHub"
	"io"
	"net"
	"os"
	"strings"
)

func Start(ch chan int) {
	sv, err := net.Listen("tcp4", "127.0.0.1:"+cts.Port)
	if err != nil {
		lock.Run(func() {
			logTb.Error(fmt.Sprintf(
				"Server listenning port %s failed (%s)", cts.Port, err,
			))
			fmt.Printf(
				"Server listenning port %s failed (%s)", cts.Port, err,
			)
			os.Exit(1)
		})
	}
	for !lock.End {
		client, err := sv.Accept()
		if err != nil {
			lock.Run(func() {
				logTb.Error("Server accepting client failed")
			})
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
					lock.Run(func() {
						logTb.Error(fmt.Sprintf(
							"Server reading client failed.\nError: %v\nCurrent input:\n%s",
							err, rq.String(),
						))
					})
					fmt.Printf(
						"Server listenning port %s failed (%s)", cts.Port, err,
					)
				}
				break
			}
		}

		rqs := rq.String()
		if rqs == "end" {
			lock.End = true
			client.Close()
			continue
		}

		rp := mainHub.Process(rqs)
		_, err = client.Write([]byte(rp))
		client.Close()

		if err != nil {
			lock.Run(func() {
				logTb.Error(fmt.Sprintf(
					"Server fail writing to client.\nError:\n%v", err,
				))
			})
		}
	}
	sv.Close()
	ch <- 0
}
