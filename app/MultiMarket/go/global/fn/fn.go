// Copyright 03-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Several functions.
package fn

import (
	"fmt"
	"math"
	"strconv"
)

// Returns true if abs(n1 - n2) < gap
func Eq(n1, n2, gap float64) (ok bool) {
	df := n1 - n2
	if df >= -gap && df <= gap {
		ok = true
	}
	return
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

// Transform a float64 to a parameter (array of int)
func ToParam(n float64) []int {
	atoi := func(s string) int {
		if r, err := strconv.Atoi(s); err == nil {
			return r
		}
		panic("'" + s + "' is not an integer")
	}
	i := strconv.Itoa(int(n * 10000.0))
	if len(i) == 6 {
		return []int{
			atoi(i[0:2]), atoi(i[2:3]), atoi(i[3:4]), atoi(i[4:5]), atoi(i[5:6]),
		}
	} else if len(i) == 5 {
		return []int{
			atoi(i[0:1]), atoi(i[1:2]), atoi(i[2:3]), atoi(i[3:4]), atoi(i[4:5]),
		}
	}
	panic("len(" + i + ") must be 5 or 6")
}

// Transform a parameter (array of int) in a float64
func fromParam(p []int) float64 {
	if len(p) == 6 {
		return float64(p[0])*10.0 +
			float64(p[1]) +
			float64(p[2])*0.1 +
			float64(p[3])*0.01 +
			float64(p[4])*0.001 +
			float64(p[5])*0.0001
	} else if len(p) == 5 {
		return float64(p[0])*1.0 +
			float64(p[1])*0.1 +
			float64(p[2])*0.01 +
			float64(p[3])*0.001 +
			float64(p[4])*0.0001
	}
	panic("len(" + strconv.Itoa(len(p)) + ") must be 5 or 6")
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
