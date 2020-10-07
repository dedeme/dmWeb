// Copyright 03-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Several functions.
package fn

import (
	"fmt"
	"math"
)

// Returns true if abs(n1 - n2) < gap
func Eq(n1, n2, gap float64) bool {
	df := n1 - n2
	if df < 0 {
		return -df < gap
	}
	return df < gap
}

// Returns n with d decimals.
//    n: A number.
//    d: Decimals number.
func Fix(n float64, d int) float64 {
	switch d {
	case 1:
		return math.Round(n*10) / 10
	case 2:
		return math.Round(n*100) / 100
	case 3:
		return math.Round(n*1000) / 1000
	case 4:
		return math.Round(n*10000) / 10000
	case 5:
		return math.Round(n*100000) / 100000
	case 6:
		return math.Round(n*1000000) / 1000000
	case 7:
		return math.Round(n*10000000) / 10000000
	case 8:
		return math.Round(n*100000000) / 100000000
	case 9:
		return math.Round(n*1000000000) / 1000000000
	default:
		return math.Round(n)
	}
}

// Returns the most duplicated value of 'vs', rounding to 4 decimals.
func MostDup(vs []float64) float64 {
	type ndupT struct {
		n   float64
		dup int
	}
	mp := map[string]ndupT{}
	for _, e := range vs {
		str := fmt.Sprintf("%.4f", e)
		mdup, ok := mp[str]
		if ok {
			mdup.dup++
		} else {
			mp[str] = ndupT{e, 1}
		}
	}

	r := ndupT{0.0, 0}
	for _, v := range mp {
		if v.dup > r.dup {
			r = v
		}
	}

	return r.n
}
