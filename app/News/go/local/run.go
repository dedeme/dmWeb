// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package local

import (
	"fmt"
	"github.com/dedeme/News/control/autoEval"
	"github.com/dedeme/News/data/cts"
	"github.com/dedeme/News/data/webSource"
	"github.com/dedeme/News/db/newss"
	"github.com/dedeme/News/net"
	"github.com/dedeme/News/test"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/sys"
)

func Process(rq string) {
	sys.Initialize(cts.APP)

	if !file.Exists(cts.WEB_DATA) {
		panic(
			"Web database is not initialized.\n" +
				"You must start the Web application.",
		)
	}

	if rq == "test" {
		test.Run()
		return
	}
	newss.LocalInit()
	for _, s := range webSource.List() {
		for _, p := range s.Pages() {
			news, err := net.Read(p)
			if err != nil {
				fmt.Printf(
					"Fail reading %v(%v):\n%v",
					s.Id(), p.Url, err.Error(),
				)
			} else {
				newss.LocalAdd(news)
			}
		}
	}
	ns := newss.LocalRead()
	autoEval.Run(ns)
	newss.LocalWrite(ns)
}
