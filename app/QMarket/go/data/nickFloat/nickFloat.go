// Copyright 18-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nick-Float pair.
package nickFloat

import (
	"github.com/dedeme/QMarket/data/nick"
)

type T struct {
	nk    *nick.T
	value float64
}

func New(nk *nick.T, value float64) *T {
	return &T{nk, value}
}

// Returns the nick.
func (nf *T) Nick() *nick.T {
	return nf.nk
}

// Returns the value.
func (nf *T) Value() float64 {
	return nf.value
}
