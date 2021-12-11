// Copyright 05-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Order to operate before execution.
package initialOrder

type T struct {
	isSale bool
	iCo    int
	pond   float64
}

// Constructor.
//    isSale: 'true' is order is for selling.
//    iCo   : Company index in a 'qtable.T'.
//    pond  : Order to buy if isSale is false.
func New(isSell bool, iCo int, pond float64) *T {
	return &T{isSell, iCo, pond}
}

// 'true' is order is for selling.
func (o *T) IsSale() bool {
	return o.isSale
}

// Company index in a 'qtable.T'.
func (o *T) ICo() int {
	return o.iCo
}

// Order to buy if isSale is false.
func (o *T) Pond() float64 {
	return o.pond
}
