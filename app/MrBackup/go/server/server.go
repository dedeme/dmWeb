// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Server process
package server

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/data/globals"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/MrBackup/server/mainHub"
	"io"
	"net"
	"os"
	"strings"
)

func Start(ch chan int) {
	sv, err := net.Listen("tcp4", "127.0.0.1:"+cts.Port)
	if err != nil {
		log.Error(fmt.Sprintf(
			"Server listenning port %s failed (%s)", cts.Port, err,
		))
		os.Exit(1)
	}
	for !globals.Stop {
		client, err := sv.Accept()
		if err != nil {
			log.Error("Server accepting client failed")
			continue
		}

		var rq strings.Builder
		buf := make([]byte, 65536)
		for {
			n, err := client.Read(buf)
			rq.Write(buf[:n])

			if err != nil {
				if err != io.EOF {
					log.Error(fmt.Sprintf(
						"Server reading client failed.\nError: %v\nCurrent input:\n%s",
						err, rq.String(),
					))
				}
				break
			}
		}

		rqs := rq.String()
		if rqs == "end" {
			globals.Stop = true
			client.Close()
			continue
		}

		rp := mainHub.Process(rqs)
		_, err = client.Write([]byte(rp))
		client.Close()

		if err != nil {
			log.Error(fmt.Sprintf("Server writing client failed.\nOutput:\n%s", rp))
		}
	}
	sv.Close()
	ch <- 0
}
