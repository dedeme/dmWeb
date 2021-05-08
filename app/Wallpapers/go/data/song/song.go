// Copyright 07-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Song record.
package song

import (
	"github.com/dedeme/golib/json"
	"math/rand"
)

type T struct {
	level  int
	sights int
	id     string
	time   int
}

// Make a new Song with level 1.
//    id: Song name.
//    time: Song time in seconds.
func New(id string, time int) *T {
	return &T{1, 0, id, time}
}

// Return the song level (1, 2, o 3).
func (p *T) Level() int {
	return p.level
}

// Return the number of sights.
func (p *T) Sights() int {
	return p.sights
}

// Returns the song name.
func (p *T) Id() string {
	return p.id
}

// Returns the song time.
func (p *T) Time() int {
	return p.time
}

func (p *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(p.level),
		json.Wi(p.sights),
		json.Ws(p.id),
		json.Wi(p.time),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Ri(),
		a[1].Ri(),
		a[2].Rs(),
		a[3].Ri(),
	}
}

// Returns a new slice with the level of song 'id' changed.
func SetLevel(songs []*T, id string, level int) (r []*T) {
	for _, e := range songs {
		if e.id == id {
			r = append(r, &T{level, e.sights, id, e.time})
		} else {
			r = append(r, e)
		}
	}
	return
}

// Returns a new slice with sights set to 0
func ResetSights(songs []*T) (r []*T) {
	for _, e := range songs {
		r = append(r, &T{e.level, 0, e.id, e.time})
	}
	return
}

// Returns true if 'songs' contains 'id' and lapse is less than 'id' total time.
func IsValid(songs []*T, id string, lapse float64) bool {
	for _, e := range songs {
		if e.id == id {
			return lapse + 1.0 < float64(e.time)
		}
	}
	return false
}

// Returns:
//    - If there is a song to hear:
//      * A new array of songs with 's'.sights incremented.
//      * The next song.
//    - If every song has been heard:
//      * An empty array.
//      * An empty string.
func Next(songs []*T) (ss []*T, s string) {
	var selSongs []*T
	for _, e := range songs {
		if e.sights < e.level {
			selSongs = append(selSongs, e)
		}
	}

	n := len(selSongs)
	if n == 0 {
		return
	}

	s = selSongs[rand.Intn(n)].id
	for _, e := range songs {
		if e.id == s {
			ss = append(ss, &T{e.level, e.sights + 1, e.id, e.time})
		} else {
			ss = append(ss, e)
		}
	}

	return
}
