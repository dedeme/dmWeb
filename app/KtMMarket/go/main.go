// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main file
package main

import (
	"github.com/dedeme/KtMMarket/cts"
	"github.com/dedeme/KtMMarket/db"
	"github.com/dedeme/KtMMarket/pgs"
	"github.com/dedeme/KtMMarket/update"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/sys"
)

func help() {
	sys.Println("Usage: KtMMarket [update | test | help | <request>]")
}

func test() {
	sys.Println("No test done")
}

func main() {
	if len(sys.Args()) != 2 {
		help()
		return
	}

	sys.Rand()
	if sys.Environ()["USER"] == "deme" {
		db.Initialize()
		switch sys.Args()[1] {
		case "update":
			update.Run()
		case "test":
			test()
		default:
			help()
		}
	} else {
		cgi.Initialize(cts.WebDir, cts.Expiration)
		sys.Println(pgs.Process(sys.Args()[1]))
	}
}
