// Copyright 19-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Relax song record.
package song

import (
	"github.com/dedeme/Wallpapers/data/sighter"
	"github.com/dedeme/golib/json"
)

type T struct {
	level  int
	sights int
	lapse  float64
	id     string
}

// Make a new Song with level 1.
func New(id string) *T {
	return &T{1, 0, 0.0, id}
}

// Return the song level (1, 2, o 3).
func (s *T) Level() int {
	return s.level
}

// Return the number of sights.
func (s *T) Sights() int {
	return s.sights
}

// Return the song lapse played.
func (s *T) Lapse() float64 {
	return s.lapse
}

// Returns the song name.
func (s *T) Id() string {
	return s.id
}

// Returna a new song with sights incremented.
func (s *T) Inc() sighter.T {
	return &T{s.level, s.sights + 1, s.lapse, s.id}
}

// Returna a new song with sights reset.
func (s *T) Reset() sighter.T {
	return &T{s.level, 0, s.lapse, s.id}
}

func (s *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(s.level),
		json.Wi(s.sights),
		json.Wd(s.lapse),
		json.Ws(s.id),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Ri(),
		a[1].Ri(),
		a[2].Rd(),
		a[3].Rs(),
	}
}

// Returns a new slice with the level of song 'id' changed.
func SetLevel(songs []*T, id string, level int) (newSongs []*T) {
	for _, e := range songs {
		if e.id == id {
			newSongs = append(newSongs, &T{level, e.sights, e.lapse, id})
		} else {
			newSongs = append(newSongs, e)
		}
	}
	return
}

// Returns a new slice with the lapse of song 'id' changed.
func SetLapse(songs []*T, id string, lapse float64) (newSongs []*T) {
	for _, e := range songs {
		if e.id == id {
			newSongs = append(newSongs, &T{e.level, e.sights, lapse, id})
		} else {
			newSongs = append(newSongs, e)
		}
	}
	return
}

// Returns the song 'id' or nil
func Get(songs []*T, id string) *T {
	for _, e := range songs {
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
