// Copyright 29-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Picture record.
package pict

import (
	"github.com/dedeme/Wallpapers/data/sighter"
	"github.com/dedeme/golib/json"
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

// Returna a new song with sights incremented.
func (p *T) Inc() sighter.T {
	return &T{p.level, p.sights + 1, p.id}
}

// Returna a new song with sights reset.
func (p *T) Reset() sighter.T {
	return &T{p.level, 0, p.id}
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
func SetLevel(picts []*T, id string, level int) (newPicts []*T) {
	for _, e := range picts {
		if e.id == id {
			newPicts = append(newPicts, &T{level, e.sights, id})
		} else {
			newPicts = append(newPicts, e)
		}
	}
	return
}

// Returns a new slice with the sights of 'picture' incremented if it is in
// the array.
func IncSights(picts []*T, picture *T) (newPicts []*T, newPicture *T) {
	newPicture = picture
	id := picture.id
	for _, e := range picts {
		if e.id == id {
			newPicture = &T{e.level, e.sights + 1, id}
			newPicts = append(newPicts, newPicture)
		} else {
			newPicts = append(newPicts, e)
		}
	}
	return
}

// Returns a new slice with sights set to 0
func ResetSights(picts []*T) (newPicts []*T) {
	for _, e := range picts {
		newPicts = append(newPicts, &T{e.level, 0, e.id})
	}
	return
}

// Returns the picture with name 'id' or nil
func Get(picts []*T, id string) *T {
	for _, e := range picts {
		if e.id == id {
			return e
		}
	}
	return nil
}

// Returns the same array as []sighter.T
func ToSighters(ss []*T) (r []sighter.T) {
	for _, s := range ss {
		r = append(r, s)
	}
	return
}

// Returns the same array as []*T
func FromSighters(ss []sighter.T) (r []*T) {
	for _, s := range ss {
		r = append(r, s.(*T))
	}
	return
}
