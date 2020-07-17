// Copyright 10-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Authomatica evaluation
package autoEval

import (
	"github.com/dedeme/News/data/newsEntry"
	"github.com/dedeme/News/data/text"
	"github.com/dedeme/News/db/eauthors"
	"github.com/dedeme/News/db/esources"
	"github.com/dedeme/News/db/evalWeights"
	"github.com/dedeme/News/db/ewords"
	"strings"
)

func evalSource(n *newsEntry.T) (r float64) {
	source := n.Source
	r = 0.5
	evs := esources.Read()
	for _, e := range evs {
		if e.Id == source {
			r = e.Eval
			return
		}
	}
	return
}

func evalAuthor(n *newsEntry.T) (r float64) {
	author := n.Author
	r = 0.5
	evs := eauthors.Read()
	for _, e := range evs {
		if e.Id == author {
			r = e.Eval
			return
		}
	}
	return
}

func evalWords(ne *newsEntry.T) float64 {
	ws := text.WordsRead(ne.Text)
	sum := float64(0.0)
	n := float64(0.0)
	for _, w := range ws {
		rn, _, err := strings.NewReader(w).ReadRune()
		if err == nil {
			r := 0.5
			evs := ewords.Read(string(rn))
			for _, e := range evs {
				if e.Id == w {
					r = e.Eval
					break
				}
			}
			sum += r
			n++
		}
	}

	if n > 0 {
		return sum / n
	}
	return 0.5
}

func Run(ns []*newsEntry.T) {
	ws := evalWeights.Read()
	for _, e := range ns {
		e.Seval = evalSource(e)
		e.Aeval = evalAuthor(e)
		e.Weval = evalWords(e)
		e.TtEval = e.Seval*ws.Source + e.Aeval*ws.Author + e.Weval*ws.Words
	}
}
