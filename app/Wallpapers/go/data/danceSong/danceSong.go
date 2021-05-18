// Copyright 17-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Dance song record.
package danceSong

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	level int
	speed int
	id    string
}

// Make a new Song with level 0.
func New(id string) *T {
	return &T{0, 0, id}
}

// Return the song level (0(unknown), 1(ok), o (well)).
func (p *T) Level() int {
	return p.level
}

// Return the song speed (0(slow), 1(fast)).
func (p *T) Speed() int {
	return p.speed
}

// Returns the song name.
func (p *T) Id() string {
	return p.id
}

func (p *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(p.level),
		json.Wi(p.speed),
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

// Returns a new slice with the level of song 'id' changed.
func SetLevel(songs []*T, id string, level int) (newSongs []*T) {
	for _, e := range songs {
		if e.id == id {
			newSongs = append(newSongs, &T{level, e.speed, id})
		} else {
			newSongs = append(newSongs, e)
		}
	}
	return
}

// Returns a new slice with the speed of song 'id' changed.
func SetSpeed(songs []*T, id string, speed int) (newSongs []*T) {
	for _, e := range songs {
		if e.id == id {
			newSongs = append(newSongs, &T{e.level, speed, id})
		} else {
			newSongs = append(newSongs, e)
		}
	}
	return
}

// Returns true if 'songs' contains a picture with name 'id'
func Contains(songs []*T, id string) bool {
	for _, e := range songs {
		if e.id == id {
			return true
		}
	}
	return false
}
