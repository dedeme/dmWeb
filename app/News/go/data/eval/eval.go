// Copyright 10-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Source evaluation data
package eval

import (
	"github.com/dedeme/News/data/ev"
	"github.com/dedeme/golib/json"
)

type T struct {
	Id   string
	Eval float64
}

func (e *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(e.Id),
		json.Wd(e.Eval),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		Id:   a[0].Rs(),
		Eval: a[1].Rd(),
	}
}

// Returns a copy of 'evs'.
// If e < 0 or e > 1, values of copy are modified to fit between [0-1]
func Normalize(evs []*T, e float64) (r []*T) {
	for _, el := range evs {
		r = append(r, &T{Id: el.Id, Eval: el.Eval})
	}
	if e > 1 {
		for _, el := range r {
			el.Eval /= e
		}
		return
	}
	if e < 0 {
		delta := -e
		e = 1 + delta
		for _, el := range evs {
			el.Eval = (el.Eval + delta) / e
		}
		return
	}
	return
}

// Update 'evs' with 'entry'
//    evs: Evalutations to update.
//    id : Identifier evaluated by user
//    ue : User evaluation. (ev.NONE, ev.UP, ev.EQ, ev.DOWN)
// It returns:
//    r      : a new array including 'entry' and with its entries normalized
//             (e.Eval >= 0, e.Eval <= 1) althoug newEval is out of limits.
//    newEval: Evaluation assined to 'id'. Its value can be less than 0 or
//             greater than 1.
func UpdateTo(evs []*T, id string, ue int) (r []*T, newEval float64) {
	newEval = float64(100.0)
	if ue == ev.NONE {
		r = evs
		return
	}

	for _, e := range evs {
		var er *T
		er.Id = e.Id
		er.Eval = e.Eval

		if e.Id == id {
			if ue == ev.UP {
				newEval = e.Eval + 0.1
			} else {
				newEval = e.Eval - 0.1
			}
			er.Eval = newEval
		}

		r = append(r, er)
	}

	if newEval > 99 {
		newEval = 0.51
		if ue == ev.DOWN {
			newEval = 0.49
		}
		r = append(r, &T{Id: id, Eval: newEval})
	} else {
		if newEval > 1 || newEval < 0 {
			r = Normalize(r, newEval)
		}
	}

	return
}
