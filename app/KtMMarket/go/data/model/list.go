// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package model

import (
	"github.com/dedeme/ktlib/arr"
)

func List() []*T {
	return []*T{
		newQfix(),
		newQmob(),
		newAppr(),
		newAppr2(),
		newMa(),
		newEa(),
		newEa2(),
		newMm(),
	}
}

// Returns a model from its identifier (APPR, QFIX...)
func FromId(id string) *T {
	md, ok := arr.Find(List(), func(md *T) bool {
		return md.Id == id
	})
	if !ok {
		panic("Model " + id + " not found")
	}
	return md
}
