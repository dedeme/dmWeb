// Copyright 17-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Dance song record.
package danceSong

import (
	"github.com/dedeme/golib/json"
)

type T struct {
	level      int
	speed      int
	id         string
	shortPlays int
	longPlays  int
}

// Make a new Song with level 0.
func New(id string) *T {
	return &T{0, 0, id, 0, 0}
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

// Returns short reproductions number.
func (p *T) ShortPlays() int {
	return p.shortPlays
}

// Returns long reproductions number.
func (p *T) LongPlays() int {
	return p.longPlays
}

func (p *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(p.level),
		json.Wi(p.speed),
		json.Ws(p.id),
		json.Wi(p.shortPlays),
		json.Wi(p.longPlays),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Ri(),
		a[1].Ri(),
		a[2].Rs(),
		a[3].Ri(),
		a[4].Ri(),
	}
}

// Returns a new slice with the level of song 'id' changed.
func SetLevel(songs []*T, id string, level int) (newSongs []*T) {
	for _, e := range songs {
		if e.id == id {
			newSongs = append(newSongs, &T{level, e.speed, id, e.shortPlays, e.longPlays})
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
			newSongs = append(newSongs, &T{e.level, speed, id, e.shortPlays, e.longPlays})
		} else {
			newSongs = append(newSongs, e)
		}
	}
	return
}

// Returns a new slice with reproductions number of song 'id' incremented.
// If its value is equals or greater than 100, every value of 'short/longPlays'
// is set to its half.
func IncrementPlays(songs []*T, id string, isShort bool) (newSongs []*T) {
	renum := false
	for _, e := range songs {
		if e.id == id {
			plays := e.shortPlays + 1
			if isShort {
				plays = e.longPlays + 1
			}
			if plays >= 100 {
				renum = true
			}
			if isShort {
				newSongs = append(newSongs, &T{e.level, e.speed, id, plays, e.longPlays})
			} else {
				newSongs = append(newSongs, &T{e.level, e.speed, id, e.shortPlays, plays})
			}
		} else {
			newSongs = append(newSongs, e)
		}
	}

	if renum {
		var news []*T
		if isShort {
			for _, e := range songs {
				news = append(news, &T{
					e.level, e.speed, e.id, e.shortPlays / 2, e.longPlays})
			}
		} else {
			for _, e := range songs {
				news = append(news, &T{
					e.level, e.speed, e.id, e.shortPlays, e.longPlays / 2})
			}
		}
		newSongs = news
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
