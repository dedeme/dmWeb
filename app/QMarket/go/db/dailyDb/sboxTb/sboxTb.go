// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Daily server selector.
package sboxTb

import (
	"github.com/dedeme/QMarket/data/server"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/db/serversTb"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"math/rand"
	"path"
)

var fpath string

func read() (r []string) {
	for _, e := range json.FromString(file.ReadAll(fpath)).Ra() {
		r = append(r, e.Rs())
	}
	return
}

func write(svs []string) {
	var a []json.T
	for _, e := range svs {
		a = append(a, json.Ws(e))
	}
	file.WriteAll(fpath, json.Wa(a).String())
}

// Initializes daily server table.
func Initialize(parent string) {
	fpath = path.Join(parent, "Sbox.tb")
	if !file.Exists(fpath) {
		write([]string{})
	}
}

// Returns the current selected server.
func GetServer() *server.T {
	svList, _ := serversTb.Read().DailyList()
	for {
		ls := read()
		if len(ls) == 0 {
			NextServer()
			continue
		}
		sv := ls[0]
		for _, e := range svList {
			if e.ShortName() == sv {
				return e
			}
		}
		logTb.Error("Daily server " + sv + " not found")
		NextServer()
	}
}

// Prepares next server as current server.
func NextServer() {
	ls := read()
	if len(ls) > 0 {
		write(ls[1:])
		return
	}
	svList, _ := serversTb.Read().DailyList()
	for _, e := range svList {
		ls = append(ls, e.ShortName())
	}
	if len(ls) == 0 {
		panic("There is no daily server in data base")
	}
	rand.Shuffle(len(ls), func(i, j int) {
		ls[i], ls[j] = ls[j], ls[i]
	})
	write(ls)
}
