// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global functions
package year

import (
	"github.com/dedeme/golib/date"
	"strconv"
)

// Returns the current sportive year.
func Current() string {
	d := date.Now()
	y := d.Year()
	if d.Month() < 8 {
		y--
	}
	return strconv.Itoa(y)
}
