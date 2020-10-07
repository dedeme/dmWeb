// Copyright 23-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Servers table.
package serversTb

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/server"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

// Auxiliar struct ------------------------------------------------------- START
type t struct {
	nextId int
	lst    []*server.T
}

func (d *t) toJs() json.T {
	var l []json.T
	for _, e := range d.lst {
		l = append(l, e.ToJs())
	}
	return json.Wa([]json.T{
		json.Wi(d.nextId),
		json.Wa(l),
	})
}

func fromJs(js json.T) *t {
	a := js.Ra()
	var l []*server.T
	for _, e := range a[1].Ra() {
		l = append(l, server.FromJs(e))
	}
	return &t{
		a[0].Ri(),
		l,
	}
}

// Auxiliar struct --------------------------------------------------------- END

var fpath string

// Auxiliar function
func write(lk sync.T, data *t) {
	file.WriteAll(fpath, data.toJs().String())
}

// Auxiliar function
func read(lk sync.T) *t {
	return fromJs(json.FromString(file.ReadAll(fpath)))
}

// Initializes table.
//    parent: Parent directory.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Servers.tb")
	if !file.Exists(fpath) {
		write(lk, &t{0, []*server.T{}})
	}
}

// Returns server list in JSON format.
//    lk: Synchronization lock.
func ReadJs(lk sync.T) json.T {
	a := json.FromString(file.ReadAll(fpath)).Ra()
	return a[1]
}

// Returns server list in JSON format.
//    lk: Synchronization lock.
func Read(lk sync.T) []*server.T {
	return read(lk).lst
}

// Adds a new server. If shortName is duplicated, it returns 'false'.
//    lk       : Synchronization lock.
//    shortName: Server short name.
func Add(lk sync.T, shortName string) bool {
	d := read(lk)
	for _, e := range d.lst {
		if e.ShortName() == shortName {
			return false
		}
	}
	id := d.nextId
	nicks := nicksTb.Nicks(lk)
	write(lk, &t{id + 1, append(d.lst, server.New(id, shortName, nicks))})
	return true
}

// Modify data of "sv". If there is not a server with 'sv.id', function
// modifies nothing and returns 'ok=false'
//    lk: Synchronization lock.
//    sv: Server to modify.
func Modify(lk sync.T, sv *server.T) (ok bool) {
	id := sv.Id()
	d := read(lk)
	var lst []*server.T
	for _, e := range d.lst {
		if e.Id() == id {
			lst = append(lst, sv)
			ok = true
			continue
		}
		lst = append(lst, e)
	}
	if ok {
		write(lk, &t{d.nextId, lst})
	}
	return
}

// Deletes a server.
//    lk: Synchronization lock.
//    id: Identifier of server to modify.
func Del(lk sync.T, id int) {
	d := read(lk)
	ok := false
	var lst []*server.T
	for _, e := range d.lst {
		if e.Id() == id {
			ok = true
			continue
		}
		lst = append(lst, e)
	}
	if ok {
		write(lk, &t{d.nextId, lst})
	}
}

// Adds a new nick to Codes of every server.
//    lk    : Synchronization lock.
//    nickId: Identifier of nick to add.
func AddNick(lk sync.T, nickId int) {
	d := read(lk)
	for _, e := range d.lst {
		e.AddCode(nickId)
	}
	write(lk, &t{d.nextId, d.lst})
}

// Removes a nick from Codes of every server.
//    lk    : Synchronization lock.
//    nickId: Identifier of nick to remove.
func DelNick(lk sync.T, nickId int) {
	d := read(lk)
	for _, e := range d.lst {
		e.RemoveCode(nickId)
	}
	write(lk, &t{d.nextId, d.lst})
}

// Modifies then nick code in Codes of a server.
//    lk      : Synchronization lock.
//    serverId: Identifier of server to modify.
//    nickId  : Identifier of nick to modify.
//    code    : New nick server code.
func ModifyNickCode(lk sync.T, serverId, nickId int, code string) {
	d := read(lk)
	for _, e := range d.lst {
		if e.Id() == serverId {
			e.ModifyCode(nickId, code)
		}
	}
	write(lk, &t{d.nextId, d.lst})
}

// Returns true if the server "serverId" has defined its 'dailyConf'.
//    lk      : Synchronization lock.
//    serverId: Identifier of server to modify.
func IsDefinedDailyConf(lk sync.T, serverId int) bool {
	for _, e := range read(lk).lst {
		if e.Id() == serverId {
			_, ok := e.DailyConf()
			return ok
		}
	}
	return false
}

// Returns true if the server "serverId" has defined its 'historicConf'.
//    lk      : Synchronization lock.
//    serverId: Identifier of server to modify.
func IsDefinedHistoricConf(lk sync.T, serverId int) bool {
	for _, e := range read(lk).lst {
		if e.Id() == serverId {
			_, ok := e.HistoricConf()
			return ok
		}
	}
	return false
}

// Returns the list of activated or selected daily configuration servers.
//    lk: Synchronization lock.
func DailyList(lk sync.T) (svs []*server.T) {
	for _, e := range read(lk).lst {
		cf, ok := e.DailyConf()
		if ok && cf.Sel() != cts.ServerStopped {
			svs = append(svs, e)
		}
	}
	return
}

// Returns the list of activated historic configuration servers and index
// of the selected server.
//    lk: Synchronization lock.
func HistoricList(lk sync.T) (svs []*server.T, selected int) {
	ix := 0
	for _, e := range read(lk).lst {
		cf, ok := e.HistoricConf()
		if ok && cf.Sel() != cts.ServerStopped {
			svs = append(svs, e)
			if cf.Sel() == cts.ServerSelected {
				selected = ix
			}
			ix++
		}
	}
	return
}
