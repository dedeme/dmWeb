// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base update.
package quotesReader

import (
	"github.com/dedeme/KtMMarket/cts"
	"github.com/dedeme/KtMMarket/data/quotes"
	"github.com/dedeme/KtMMarket/fns"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/math"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/str"
)

func normalize(nks []string, vals [][]float64) {
	rows := len(vals)
	cols := len(vals[0])
	for r := 1; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if vals[r][c] < 0 {
				vals[r][c] = vals[r-1][c]
			}
		}
	}
	for r := rows - 2; r >= 0; r-- {
		for c := 0; c < cols; c++ {
			if vals[r][c] < 0 {
				vals[r][c] = vals[r+1][c]
			}
		}
	}
	for c := 0; c < cols; c++ {
		if vals[0][c] < 0 {
			panic("Every value of company " + nks[c] + " is less than 0")
		}
	}
}

// Read data from KtMarket
func Read() *quotes.T {
	nksTb := file.Read(path.Cat(cts.KtMarketDataDir, "nicks.tb"))
	nksJs := js.Ra(nksTb)[2]
	nks := arr.Map(arr.Filter(js.Ra(nksJs), func(j string) bool {
		return js.Rb(js.Ra(j)[2])
	}), func(j string) string {
		return js.Rs(js.Ra(j)[1])
	})
	arr.Sort(nks, func(n1, n2 string) bool {
		return n1 < n2
	})
	nCos := len(nks)

	qsTb := file.Read(path.Cat(cts.KtMarketDataDir, "quotes", nks[0]+".tb"))
	dates := arr.Map(str.Split(qsTb, "\n"), func(l string) string {
		return l[:8]
	})
	arr.ReverseIn(dates)

	opens := make([][]float64, len(dates))
	closes := make([][]float64, len(dates))
	maxs := make([][]float64, len(dates))

	for i := 0; i < len(dates); i++ {
		opens[i] = make([]float64, nCos)
		closes[i] = make([]float64, nCos)
		maxs[i] = make([]float64, nCos)
	}

	for coIx, co := range nks {
		qsTb := file.Read(path.Cat(cts.KtMarketDataDir, "quotes", co+".tb"))
		for dtIx, l := range arr.Reverse(str.Split(qsTb, "\n")) {
			ps := str.Split(l, ":")
			opens[dtIx][coIx] = math.FromStr(ps[1])
			closes[dtIx][coIx] = math.FromStr(ps[2])
			maxs[dtIx][coIx] = math.FromStr(ps[3])
		}
	}

	normalize(nks, opens)
	normalize(nks, closes)
	normalize(nks, maxs)

	return quotes.New(fns.LastSunday(), nks, dates, opens, closes, maxs)
}
