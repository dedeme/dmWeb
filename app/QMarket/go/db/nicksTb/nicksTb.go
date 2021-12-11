// Copyright 18-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nicks table.
package nicksTb

import (
	"github.com/dedeme/QMarket/data/nick"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initializes table.
//    parent: Parent directory.
func Initialize(parent string) {
	fpath = path.Join(parent, "Nicks.tb")
	if !file.Exists(fpath) {
		Write(nick.NewTable())
	}
}

// Writes table.
func Write(t *nick.TableT) {
	file.WriteAll(fpath, t.ToJs().String())
}

// Reads table
func Read() *nick.TableT {
	return nick.TableFromJs(json.FromString(file.ReadAll(fpath)))
}

/*
// Returns nicks list.
//    lk: Synchronization lock.
func Nicks(lk sync.T) []*nick.T {
	return read(lk).lst
}

// Returns the list of selected nicks.
//    lk: Synchronization lock.
func SelectedNicks(lk sync.T) []*nick.T {
	var l []*nick.T
	for _, e := range read(lk).lst {
		if e.IsSel() {
			l = append(l, e)
		}
	}
	return l
}

// Returns the nick which id is 'nickId'. If such nick does not exist,
// 'ok' is 'false'
//    lk    : Synchronization lock.
//    nickId: Nick identifier.
func GetNick(lk sync.T, nickId int) (nk *nick.T, ok bool) {
	for _, e := range read(lk).lst {
		if e.Id() == nickId {
			nk = e
			ok = true
			return
		}
	}
	return
}

// Adds a new nick if it is not duplicated. In such case it logs an error
// and returns "false". If the operation succeeds, the nick identifier is
// returned.
//    lk    : Synchronization lock.
//    nickName: Nick name.
func Add(lk sync.T, nickName string) (nickId int, ok bool) {
	d := read(lk)
	for _, e := range d.lst {
		if e.Name() == nickName {
			log.Error(lk, "Nick name "+nickName+" is duplicated")
			return
		}
	}

	nickId = d.nextId
	nk := nick.New(nickId, nickName)
	d.nextId++
	d.lst = append(d.lst, nk)
	write(lk, d)
	ok = true
	return
}

// Removes the nick which id is 'nickId'.
//    lk    : Synchronization lock.
//    nickId: Nick identifier.
func Del(lk sync.T, nickId int) {
	d := read(lk)
	var l []*nick.T
	for _, e := range d.lst {
		if e.Id() != nickId {
			l = append(l, e)
		}
	}
	d.lst = l
	write(lk, d)
}

// Modifies the name of nick with id "nickId" and returns its old name.
//
// If the name is duplicated or nickId does no exist, it returns 'ok = false'.
//    lk    : Synchronization lock.
//    nickId: Nick identifier.
//    name  : New name.
func SetName(lk sync.T, nickId int, name string) (oldName string, ok bool) {
	d := read(lk)
	var l []*nick.T
	for _, e := range d.lst {
		if e.Id() == nickId {
			oldName = e.Name()
			e.SetName(name)
			l = append(l, e)
		} else {
			if e.Name() == name {
				return
			}
			l = append(l, e)
		}
	}
	d.lst = l
	write(lk, d)
	ok = true
	return
}

// Selects/Deselects the nick with id "nickId"
//    lk    : Synchronization lock.
//    nickId: Nick identifier.
//    value : 'true' If selected.
func SetIsSel(lk sync.T, nickId int, value bool) {
	d := read(lk)
	var l []*nick.T
	for _, e := range d.lst {
		if e.Id() == nickId {
			e.SetSel(value)
		}
		l = append(l, e)
	}
	d.lst = l
	write(lk, d)
}

// Returns the nick model.
//
// If it is not defined, returns the first nick.
//
// If there are not nicks in table, returns 'ok = false'.
//    lk    : Synchronization lock.
func GetModel(lk sync.T) (nk *nick.T, ok bool) {
	d := read(lk)
	if len(d.lst) == 0 {
		return
	}
	nk = d.lst[0]
	for _, e := range d.lst {
		if e.Id() == d.model {
			nk = e
			break
		}
	}
	ok = true
	return
}

// Set the nick with id "nickId" as nick model.
//    lk    : Synchronization lock.
//    nickId: Nick identifier.
func SetModel(lk sync.T, nickId int) {
	d := read(lk)
	d.model = nickId
	write(lk, d)
}
*/
