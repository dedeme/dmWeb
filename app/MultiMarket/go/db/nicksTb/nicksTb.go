// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nicks table.
package nicksTb

import (
	"github.com/dedeme/MultiMarket/data/nick"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

// Auxiliar struct ------------------------------------------------------- START
type t struct {
	nextId int
	model  int
	lst    []*nick.T
}

func (d *t) toJs() json.T {
	var l []json.T
	for _, e := range d.lst {
		l = append(l, e.ToJs())
	}
	return json.Wa([]json.T{
		json.Wi(d.nextId),
		json.Wi(d.model),
		json.Wa(l),
	})
}

func fromJs(js json.T) *t {
	a := js.Ra()
	var l []*nick.T
	for _, e := range a[2].Ra() {
		l = append(l, nick.FromJs(e))
	}
	return &t{
		a[0].Ri(),
		a[1].Ri(),
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

// Initializes calendar table.
//    parent: Parent directory.
func Initialize(parent string) {
	fpath = path.Join(parent, "Nicks.tb")
	if !file.Exists(fpath) {
		sync.Run(func(lk sync.T) {
			write(lk, &t{0, -1, []*nick.T{}})
		})
		return
	}
}

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
		if e.IsSel {
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
		if e.Id == nickId {
			nk = e
			ok = true
			return
		}
	}
	return
}

// Returns list and model of nicks.
//    lk: Synchronization lock.
func Data(lk sync.T) (model int, lst []*nick.T) {
  d := read(lk)
  model = d.model
  lst = d.lst
  return
}

func Add(lk sync.T, nickName string) (nickId int, ok bool) {
  d := read(lk)
  for _, e := range d.lst {
    if e.Name == nickName {
      log.Error(lk, "Nick name " + nickName + " is duplicated")
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

