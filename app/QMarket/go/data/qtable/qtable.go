// Copyright 19-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Table with nicks names and its quotes.values (opens, closes...).
package qtable

import (
	"github.com/dedeme/QMarket/data/matrix"
)

type T struct {
	nicks  []string
	values [][]float64
}

// Constructor
//    nicks: Nicks names.
//    values: matrix[rows][cols] where:
//            - rows are 'days' (from before to after).
//            - cols are companies.
func New(nicks []string, values [][]float64) *T {
	return &T{nicks, values}
}

// Niks names
func (q *T) Nicks() []string {
	return q.nicks
}

func (q *T) Values() [][]float64 {
	return q.values
}

// Returns a matrix[days][1] values of a nick, from before to after. If
// 'nick' is not found, returns 'ok=false'
//    nick : Nick name to search.
func (table *T) NickValues(nick string) (mx [][]float64, ok bool) {
	ix := -1
	for i, e := range table.nicks {
		if e == nick {
			ix = i
			break
		}
	}
	if ix == -1 {
		return
	}
	mx = matrix.GetCol(table.values, ix)
	ok = true
	return
}

// Returns a matrix[days][1] values of a nick, from before to after, adding
// at the end 'value' and removing the first value. If 'nick' is not found,
// returns 'ok=false'
//    table: All the companies data.
//    nick : Nick name to search.
//    value: Value to add.
func (table *T) NickValuesAdd(
	nick string, value float64,
) (mx [][]float64, ok bool) {
	mx, ok = table.NickValues(nick)
	if ok {
		mx = append(mx, []float64{value})[1:]
		return
	}
	return
}

// Returns the last valid value (>= 0) of a company with index 'iCo'.
//    matrix: Value of a 'table.values'.
//    iCo   : Company index (column value).
func LastRowOk(mx [][]float64, iCo int) float64 {
	for i := len(mx) - 1; i >= 0; i-- {
		v := mx[i][iCo]
		if v >= 0 {
			return v
		}
	}
	panic("All values are < 0")
}

// Returns a float64[length col][1] from a column of values.
//    table: Table to extract the column
//    ix   : Index of table
func GetCol(table [][]float64, ix int) [][]float64 {
	size := len(table)
	r := make([][]float64, size)
	for i := 0; i < size; i++ {
		r[i] = []float64{table[i][ix]}
	}
	return r
}
