// Copyright 10-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Authomatica evaluation
package userEval

import (
	"github.com/dedeme/News/data/ev"
	"github.com/dedeme/News/data/eval"
	"github.com/dedeme/News/data/newsEntry"
	"github.com/dedeme/News/data/text"
	"github.com/dedeme/News/db/eauthors"
	"github.com/dedeme/News/db/esources"
	"github.com/dedeme/News/db/evalWeights"
	"github.com/dedeme/News/db/ewords"
	"github.com/dedeme/News/db/newss"
	"strings"
)

func update(
	evs []*eval.T, id string, inc float64,
) (newEvs []*eval.T, normalize bool) {
	newEvs = evs
	for _, e := range newEvs {
		if e.Id == id {
			e.Eval += inc
			if e.Eval > 1 || e.Eval < 0 {
				normalize = true
			}
			return
		}
	}
	newEvs = append(newEvs, &eval.T{Id: id, Eval: 0.5 + inc})
	return
}

func normalize(evs []*eval.T) {
	for _, e := range evs {
		e.Eval = (e.Eval + 0.1) * 0.8
	}
}

func evalSource(n *newsEntry.T, inc float64) {
	evs := esources.Read()
	evs, nm := update(evs, n.Source, inc)
	if nm {
		normalize(evs)
	}
	esources.Write(evs)
}

func evalAuthor(n *newsEntry.T, inc float64) {
	evs := eauthors.Read()
	evs, nm := update(evs, n.Author, inc)
	if nm {
		normalize(evs)
	}
	eauthors.Write(evs)
}

func evalWords(n *newsEntry.T, inc float64) {
	ws := text.WordsRead(n.Text)
	for _, w := range ws {
		rn, _, err := strings.NewReader(w).ReadRune()
		if err == nil {
			evs := ewords.Read(string(rn))
			evs, nm := update(evs, w, inc)
			if nm {
				ewords.Write(string(rn), evs)
				for _, e := range ewords.ReadLetters() {
					evs2 := ewords.Read(e)
					normalize(evs2)
				}
				return
			}
			ewords.Write(string(rn), evs)
		}
	}
}

// Evaluates a news entry. 'e' can be ev.UP, ev.DOWN or ev.NONE.
func Run(entry *newsEntry.T, e int) {
	ew := evalWeights.Read()
	ue := entry.UserEval
	inc := float64(100.0)
	if ue == ev.NONE {
		if e == ev.UP {
			inc = 0.01
			ew = ew.UpdateTo(entry)
		} else if e == ev.DOWN {
			inc = -0.01
			ew = ew.UpdateTo(entry)
		}
	} else if ue == ev.UP {
		if e == ev.NONE {
			inc = -0.01
			entry.UserEval = ev.DOWN
			ew = ew.UpdateTo(entry)
			entry.UserEval = ev.NONE
		} else if e == ev.DOWN {
			inc = -0.02
			ew = ew.UpdateTo(entry)
			ew = ew.UpdateTo(entry)
		}
	} else {
		if e == ev.NONE {
			inc = 0.01
			entry.UserEval = ev.UP
			ew = ew.UpdateTo(entry)
			entry.UserEval = ev.NONE
		} else if e == ev.UP {
			inc = 0.02
			ew = ew.UpdateTo(entry)
			ew = ew.UpdateTo(entry)
		}
	}
	if inc > 99 {
		return
	}

	entry.UserEval = e
	newss.Modify(entry)

	evalWeights.Write(ew)

	evalSource(entry, inc)
	evalAuthor(entry, inc)
	evalWords(entry, inc)

}
