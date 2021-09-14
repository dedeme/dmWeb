// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Year data base initialization
package year

import (
	"github.com/dedeme/Bet1x2/db/year/clubs"
	"github.com/dedeme/Bet1x2/db/year/matchdays"
	"github.com/dedeme/golib/file"
)

// Create year data base. 'dpath' is for expample ".../2021"
func Mk(dpath string) {
	file.Mkdir(dpath)
	clubs.Mk(dpath)
	matchdays.Mk(dpath)
}

