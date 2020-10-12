// Copyright 08-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Managers table.
package managersTb

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/manager"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

func write(lk sync.T, managers []*manager.T) {
	var a []json.T
	for _, e := range managers {
		a = append(a, e.ToJs())
	}
	file.WriteAll(fpath, json.Wa(a).String())
}

// Initializes calendar table.
//    parent: Parent directory.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Managers.tb")
	if !file.Exists(fpath) {
		var mgs []*manager.T
		for i := 0; i < cts.Managers; i++ {
			mgs = append(mgs, manager.New())
		}
		write(lk, mgs)
	}
}

// Returns managers list.
//    lk: Synchronization lock.
func Read(lk sync.T) (r []*manager.T) {
	for _, e := range json.FromString(file.ReadAll(fpath)).Ra() {
		r = append(r, manager.FromJs(e))
	}
	return
}

// Sets the model base of a manager and regularize company models.
//    lk    : Synchronization lock.
//    man   : Manager id.
//    model : Model to set.
//    params: Model parameters.
func SetBase(lk sync.T, man int, model *fmodel.T, params []float64) {
	mgs := Read(lk)
	mg := mgs[man]
	mg.Base = manager.NewEntryFrom(model, params)
	write(lk, mgs)
	Regularize(lk, man)
}

// Sets the model of a company of an investor.
//    lk      : Synchronization lock.
//    man     : Manager id.
//    nickName: Company nick.
//    model   : Model to set.
//    params  : Model parameters.
func SetNick(
	lk sync.T, man int, nickName string, model *fmodel.T, params []float64,
) {
	mgs := Read(lk)
	mg := mgs[man]
	mg.Nicks()[nickName] = manager.NewEntryFrom(model, params)
	write(lk, mgs)
}

// Regularizes company models.
//    lk      : Synchronization lock.
//    man     : Manager id.
func Regularize(lk sync.T, man int) {
	mgs := Read(lk)
	mg := mgs[man]
	baseCf := mg.Base
	nicks := nicksTb.Nicks(lk)
	for _, nk := range nicks {
		nkCf, ok := mg.Nicks()[nk.Name()]
		if !ok {
			mg.Nicks()[nk.Name()] = baseCf
			continue
		}
		if !nkCf.Eq(baseCf) {
			closes, ok := quotesDb.Closes(lk).NickValues(nk.Name())
			if ok {
				lastCl := closes[len(closes)-1][0]
				refs := baseCf.Model().Refs(closes, baseCf.Params())
				lastBaseRf := refs[len(refs)-1]
				refs = nkCf.Model().Refs(closes, nkCf.Params())
				lastNkRf := refs[len(refs)-1]
				if (lastBaseRf > lastCl && lastNkRf > lastCl) ||
					(lastBaseRf < lastCl && lastNkRf < lastCl) {
					mg.Nicks()[nk.Name()] = baseCf
				}
			} else {
				mg.Nicks()[nk.Name()] = baseCf
			}
		}
	}

	var keys []string
	for k := range mg.Nicks() {
		keys = append(keys, k)
	}
	for _, e := range keys {
		_, ok := mg.Nicks()[e]
		if !ok {
			delete(mg.Nicks(), e)
		}
	}

	write(lk, mgs)
}
