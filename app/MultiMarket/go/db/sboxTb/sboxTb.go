// Copyright 08-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Daily server selector.
package sboxTb

import (
	"github.com/dedeme/MultiMarket/data/server"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/serversTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"math/rand"
	"path"
)

var fpath string

func read(lk sync.T) (r []string) {
	for _, e := range json.FromString(file.ReadAll(fpath)).Ra() {
		r = append(r, e.Rs())
	}
	return
}

func write(lk sync.T, svs []string) {
	var a []json.T
	for _, e := range svs {
		a = append(a, json.Ws(e))
	}
	file.WriteAll(fpath, json.Wa(a).String())
}

// Initializes daily server table.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Sbox.tb")
	if !file.Exists(fpath) {
		write(lk, []string{})
	}
}

// Returns the current selected server.
//    lk: Synchronization lock.
func GetServer(lk sync.T) *server.T {
	svList := serversTb.DailyList(lk)
	for {
		ls := read(lk)
		if len(ls) == 0 {
			NextServer(lk)
			continue
		}
		sv := ls[0]
		for _, e := range svList {
			if e.ShortName() == sv {
				return e
			}
		}
		log.Error(lk, "Daily server "+sv+" not found")
		NextServer(lk)
	}
}

// Prepares next server as current server.
//    lk: Synchronization lock.
func NextServer(lk sync.T) {
	ls := read(lk)
	if len(ls) > 0 {
		write(lk, ls[1:])
		return
	}
	for _, e := range serversTb.DailyList(lk) {
		ls = append(ls, e.ShortName())
	}
	if len(ls) == 0 {
		panic("There is no daily server in data base")
	}
	rand.Shuffle(len(ls), func(i, j int) {
		ls[i], ls[j] = ls[j], ls[i]
	})
	write(lk, ls)
}
