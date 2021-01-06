// Copyright 11-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Register to initialize flea calculus.
package refPos

type T struct {
	ref   float64
	toBuy bool
}

// Constructor.
//    ref: Initial reference.
//    toBuy: If is 'true', initial position is to buy. Otherwise is to sell.
func New(ref float64, toBuy bool) *T {
	return &T{ref, toBuy}
}

// Initial reference.
func (r *T) Ref() float64 {
	return r.ref
}

// If is 'true', initial position is to buy. Otherwise is to sell.
func (r *T) ToBuy() bool {
	return r.toBuy
}
