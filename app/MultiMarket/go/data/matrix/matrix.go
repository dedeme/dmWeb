// Copyright 22-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Matrix of values.
package matrix

// Create an array of rows x cols elements set to 0
func New2(rows, cols int) (r [][]float64) {
	for i := 0; i < rows; i++ {
		r = append(r, make([]float64, cols))
	}
	return
}

// Returns a matrix[rows][1] from a matrix column.
//    mx: Matrix to extract column.
//    ix: Column index
func GetCol(mx [][]float64, ix int) (r [][]float64) {
	for i := 0; i < len(mx); i++ {
		r = append(r, []float64{mx[i][ix]})
	}
	return
}
