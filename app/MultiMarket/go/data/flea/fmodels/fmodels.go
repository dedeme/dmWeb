// Copyright 08-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Flea models
package fmodels

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/flea/models/appr"
	"github.com/dedeme/MultiMarket/data/flea/models/dif"
	"github.com/dedeme/MultiMarket/data/flea/models/ga"
	"github.com/dedeme/MultiMarket/data/flea/models/ijmp"
	"github.com/dedeme/MultiMarket/data/flea/models/incr"
	"github.com/dedeme/MultiMarket/data/flea/models/jmp2"
	"github.com/dedeme/MultiMarket/data/flea/models/jump"
	"github.com/dedeme/MultiMarket/data/flea/models/mm"
)

// Returns the model list.
func List() []*fmodel.T {
	return []*fmodel.T{
		appr.Mk(),
		ga.Mk(),
		incr.Mk(),
		mm.Mk(),
		dif.Mk(),
		jump.Mk(),
		jmp2.Mk(),
		ijmp.Mk(),
	}
}

// Returns a model from its identifier. If the model is not found "ok==false".
func GetModel(modelId string) (md *fmodel.T, ok bool) {
	for _, e := range List() {
		if e.Id() == modelId {
			md = e
			ok = true
			return
		}
	}
	return
}
