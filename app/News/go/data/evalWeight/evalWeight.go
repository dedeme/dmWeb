// Copyright 10-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Evaluation data
package evalWeight

import (
	"github.com/dedeme/News/data/cts"
	"github.com/dedeme/News/data/ev"
	"github.com/dedeme/News/data/newsEntry"
	"github.com/dedeme/golib/json"
)

type T struct {
	Source float64
	Author float64
	Words  float64
}

func (e *T) UpdateTo(entry *newsEntry.T) (r *T) {
  r = &T{
	 Source: e.Source,
	 Author : e.Author,
	 Words :e.Words,
 }

	ue := entry.UserEval
	if ue == ev.NONE {
		return r
	}
	if ue == ev.UP {
		if r.Source > cts.THIRD {
			r.Source += 0.01
		} else {
			r.Source -= 0.01
		}
		if r.Author > cts.THIRD {
			r.Author += 0.01
		} else {
			r.Author -= 0.01
		}
		if r.Words > cts.THIRD {
			r.Words += 0.01
		} else {
			r.Words -= 0.01
		}
	}
	if ue == ev.DOWN {
		if r.Source < cts.THIRD {
			r.Source += 0.01
		} else {
			r.Source -= 0.01
		}
		if r.Author < cts.THIRD {
			r.Author += 0.01
		} else {
			r.Author -= 0.01
		}
		if r.Words < cts.THIRD {
			r.Words += 0.01
		} else {
			r.Words -= 0.01
		}
	}

	sum := r.Source + r.Author + r.Words
	r.Source /= sum
	r.Author /= sum
	r.Words /= sum
	return r
}

func (e *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wd(e.Source),
		json.Wd(e.Author),
		json.Wd(e.Words),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		Source: a[0].Rd(),
		Author: a[1].Rd(),
		Words:  a[2].Rd(),
	}
}
