// Copyright 29-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Picture record.
package pict

import (
	"github.com/dedeme/golib/json"
	"math/rand"
)

type T struct {
	level  int
	sights int
	id     string
}

// Make a new Pict with level 1.
func New(id string) *T {
	return &T{1, 0, id}
}

// Return the picture level (1, 2, o 3).
func (p *T) Level() int {
	return p.level
}

// Return the number of sights.
func (p *T) Sights() int {
	return p.sights
}

// Returns the picture name.
func (p *T) Id() string {
	return p.id
}

func (p *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(p.level),
		json.Wi(p.sights),
		json.Ws(p.id),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Ri(),
		a[1].Ri(),
		a[2].Rs(),
	}
}

// Returns a new slice with the level of picture 'id' changed.
func SetLevel(picts []*T, id string, level int) (r []*T) {
	for _, e := range picts {
		if e.id == id {
			r = append(r, &T{level, e.sights, id})
		} else {
			r = append(r, e)
		}
	}
	return
}

// Returns a new slice with sights set to 0
func ResetSights(picts []*T) (r []*T) {
	for _, e := range picts {
		r = append(r, &T{e.level, 0, e.id})
	}
	return
}

// Returns true if 'picts' contains a picture with name 'id'
func Contains(picts []*T, id string) bool {
	for _, e := range picts {
		if e.id == id {
			return true
		}
	}
	return false
}

// Returns:
//    - If there is a picture to see:
//      * A new array of pictures with 'p'.sights incremented.
//      * The next picture.
//    - If every picture has been saw:
//      * An empty array.
//      * An empty string.
func Next(picts []*T) (ps []*T, p string) {
	var selPicts []*T
	for _, e := range picts {
		if e.sights < e.level {
			selPicts = append(selPicts, e)
		}
	}

	n := len(selPicts)
	if n == 0 {
		return
	}

	p = selPicts[rand.Intn(n)].id
	for _, e := range picts {
		if e.id == p {
			ps = append(ps, &T{e.level, e.sights + 1, e.id})
		} else {
			ps = append(ps, e)
		}
	}

	return
}
