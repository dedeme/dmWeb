// Copyright 09-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Quote data
package quote

import (
	"github.com/dedeme/ktlib/arr"
)

// Remove -1 values.
func normalize(vals []float64) {
	rows := len(vals)
	for r := 1; r < rows; r++ {
		if vals[r] < 0 {
			vals[r] = vals[r-1]
		}
	}
	for r := rows - 2; r >= 0; r-- {
		if vals[r] < 0 {
			vals[r] = vals[r+1]
		}
	}
	if vals[0] < 0 {
		panic("Normalizing quotes, every value is less than 0")
	}
}

// Returns dates of quotes for before to after
//    qs: Quotes from after to before.
func Dates(qs []*T) (dates []string) {
	qs2 := arr.Reverse(qs)
	dates = make([]string, len(qs2))
	for i, q := range qs2 {
		dates[i] = q.date
	}
	return
}

// Returns closes of quotes for before to after
//    qs: Quotes from after to before.
func Closes(qs []*T) (closes []float64) {
	qs2 := arr.Reverse(qs)
	closes = make([]float64, len(qs2))
	for i, q := range qs2 {
		closes[i] = q.close
	}
	normalize(closes)
	return
}

// Returns opens of quotes for before to after
//    qs: Quotes from after to before.
func Opens(qs []*T) (opens []float64) {
	qs2 := arr.Reverse(qs)
	opens = make([]float64, len(qs2))
	for i, q := range qs2 {
		opens[i] = q.open
	}
	normalize(opens)
	return
}

// Returns maximuns of quotes for before to after
//    qs: Quotes from after to before.
func Maxs(qs []*T) (maxs []float64) {
	qs2 := arr.Reverse(qs)
	maxs = make([]float64, len(qs2))
	for i, q := range qs2 {
		maxs[i] = q.max
	}
	normalize(maxs)
	return
}
