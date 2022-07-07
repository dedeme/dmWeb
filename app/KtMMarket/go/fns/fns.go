// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global functions
package fns

import (
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/math"
	"github.com/dedeme/ktlib/time"
)

// Retuns the last Sunday date in format 'yyyymmdd'.
func LastSunday() string {
	today := time.Now()
	wd := time.Weekday(today)
	return time.ToStr(time.AddDays(today, -wd))
}

// Returns 'true' if params1 is equals to params2.
func EqParams(params1, params2 []float64) bool {
	return arr.Eqf(params1, params2, func(n1, n2 float64) bool {
		return math.Eq(n1, n2, 0.0000001)
	})
}

// Returns a new []float64 with the same values as 'params'.
func CopyParams(params []float64) []float64 {
	r := make([]float64, len(params))
	for i, p := range params {
		r[i] = p
	}
	return r
}
