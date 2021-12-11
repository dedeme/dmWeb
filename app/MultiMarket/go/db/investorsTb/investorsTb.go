// Copyright 08-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investors table.
package investorsTb

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/investor"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/db/refsDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

func write(lk sync.T, investors []*investor.T) {
	var a []json.T
	for _, e := range investors {
		a = append(a, e.ToJs())
	}
	file.WriteAll(fpath, json.Wa(a).String())
}

// Initializes calendar table.
//    parent: Parent directory.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Investors.tb")
	if !file.Exists(fpath) {
		var invs []*investor.T
		for i := 0; i < cts.Investors; i++ {
			invs = append(invs, investor.New())
		}
		write(lk, invs)
	}
}

// Returns managers list.
//    lk: Synchronization lock.
func Read(lk sync.T) (r []*investor.T) {
	for _, e := range json.FromString(file.ReadAll(fpath)).Ra() {
		r = append(r, investor.FromJs(e))
	}
	return
}

// Sets the model base of a manager and regularize company models.
//    lk   : Synchronization lock.
//    man  : Manager id.
//    model: Model to set.
//    param: Model parameter.
func SetBase(lk sync.T, man int, model *fmodel.T, param float64) {
	mgs := Read(lk)
	mg := mgs[man]
	mg.Base = investor.NewEntryFrom(model, param)
	write(lk, mgs)
	Regularize(lk, man)
}

// Sets the model of a company of an investor.
//    lk      : Synchronization lock.
//    man     : Manager id.
//    nickName: Company nick.
//    model   : Model to set.
//    param   : Model parameter.
func SetNick(
	lk sync.T, man int, nickName string, model *fmodel.T, param float64,
) {
	mgs := Read(lk)
	mg := mgs[man]
	mg.Nicks()[nickName] = investor.NewEntryFrom(model, param)
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
			dates := quotesDb.Dates(lk)
			closes, ok := quotesDb.Closes(lk).NickValues(nk.Name())
			if ok {
				lastCl := closes[len(closes)-1][0]
				refs := refsDb.MkRefs(
					lk, nk.Name(), dates, closes, baseCf.Model(), baseCf.Param(),
				)
				lastBaseRf := refs[len(refs)-1]
				refs = refsDb.MkRefs(
					lk, nk.Name(), dates, closes, nkCf.Model(), nkCf.Param(),
				)
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
		ok := false
		for _, nk := range nicks {
			if e == nk.Name() {
				ok = true
				break
			}
		}

		if !ok {
			delete(mg.Nicks(), e)
		}
	}

	refsDb.Purge(lk, mgs)

	write(lk, mgs)
}
