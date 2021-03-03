// Copyright 02-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting references data base.
package refsDb

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/flea/refPos"
	"github.com/dedeme/MultiMarket/data/manager"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
	"strings"
)

var fpath string

// Initializes refs data base.
//    parent: Parent directory.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "refs")
	if !file.Exists(fpath) {
		file.Mkdir(fpath)
	}
}

// Stringify params
func toKey(model *fmodel.T, params []float64) string {
	ss := []string{model.Id()}
	for _, p := range params {
		ss = append(ss, json.Wd(p).String())
	}
	return strings.Join(ss, "-")
}

func readNicks(lk sync.T) []string {
	var r []string
	for _, f := range file.List(fpath) {
		n := f.Name()
		r = append(r, n[:len(n)-3])
	}
	return r
}

func readNick(lk sync.T, nick string) (js json.T, ok bool) {
	f := path.Join(fpath, nick+".tb")
	if file.Exists(f) {
		js = json.FromString(file.ReadAll(f))
		ok = true
	}
	return
}

func read(
	lk sync.T, nick string, modelKey string,
) (dates []string, refs []float64, ok bool) {
	data, ok2 := readNick(lk, nick)
	if ok2 {
		e, ok3 := data.Ro()[modelKey]
		if ok3 {
			a := e.Ra()
			for _, d := range a[0].Ra() {
				dates = append(dates, d.Rs())
			}
			for _, r := range a[1].Ra() {
				refs = append(refs, r.Rd())
			}
			ok = true
		}
	}
	return
}

// If len(data) == 0, nick table is deleted
func writeNick(lk sync.T, nick string, data map[string]json.T) {
	f := path.Join(fpath, nick+".tb")
	if len(data) == 0 {
		file.Remove(f)
		return
	}
	file.WriteAll(f, json.Wo(data).String())
}

func write(
	lk sync.T, nick string, modelKey string, dates []string, refs []float64,
) {
	var datesJs []json.T
	for _, e := range dates {
		datesJs = append(datesJs, json.Ws(e))
	}

	var refsJs []json.T
	for _, e := range refs {
		refsJs = append(refsJs, json.Wd(e))
	}

	dateRefs := json.Wa([]json.T{json.Wa(datesJs), json.Wa(refsJs)})

	newData := map[string]json.T{}
	data, ok := readNick(lk, nick)
	if ok {
		newData = data.Ro()
	}
	newData[modelKey] = dateRefs

	file.WriteAll(path.Join(fpath, nick+".tb"), json.Wo(newData).String())
}

// Makes, saves and returns references of a tracked company.
//    lk    : Synchronization lock.
//    nick  : Company nick
//    dates : Closes dates from before to after.
//    closes: [days][1]float64 with company closes from before to after.
//    model : Model to calculate references.
//    params: Model parameters.
func MkRefs(
	lk sync.T, nick string, dates []string, closes [][]float64,
	model *fmodel.T, params []float64,
) (refs []float64) {

	defer func() {
		if r := recover(); r != nil {
			log.Error(lk, fmt.Sprint(r))
			refs = model.Refs(closes, params, nil)
			return
		}
	}()

	if len(dates) != len(closes) {
		panic(fmt.Sprintf(
			"dDates number (%v) and closes number (%v) are different.\n",
			len(dates), len(closes),
		))
	}

	if !model.WithInit() {
		return model.Refs(closes, params, nil)
	}

	ds, rs, ok := read(lk, nick, toKey(model, params))

	if !ok {
		refs = model.Refs(closes, params, nil)
		write(lk, nick, toKey(model, params), dates, refs)
		return
	}

	end := len(rs) / 2
	var date string
	var ref float64
	for i, r := range rs {
		refs = append(refs, r)
		if i > end {
			date = ds[i]
			ref = r
			break
		}
	}

	ix := -1
	for i, d := range dates {
		if d == date {
			ix = i
			break
		}
	}

	if ix == -1 {
		refs = model.Refs(closes, params, nil)
		write(lk, nick, toKey(model, params), dates, refs)
		return
	}

	toBuy := ref > closes[ix][0]
	refs2 := model.Refs(closes[ix+1:], params, refPos.New(ref, toBuy))
	refs = append(rs[:ix+1], refs2...)
	write(lk, nick, toKey(model, params), dates, refs)

	return
}

// Removes references not used.
//    lk: Synchronization lock.
func Purge(lk sync.T, managers []*manager.T) {
	used := map[string]bool{}
	for _, man := range managers {
		md := man.Base
		used[toKey(md.Model(), md.Params())] = true
		for _, v := range man.Nicks() {
			used[toKey(v.Model(), v.Params())] = true
		}
	}

	for _, nk := range readNicks(lk) {
		js, ok := readNick(lk, nk)
		if ok {
			mp := js.Ro()
			var toDel []string
			for k := range mp {
				_, exists := used[k]
				if !exists {
					toDel = append(toDel, k)
				}
			}
			if len(toDel) > 0 {
				for _, k := range toDel {
					delete(mp, k)
					log.Info(lk, fmt.Sprintf("References of %v %v deleted", nk, k))
				}
				writeNick(lk, nk, mp)
			}
		}
	}

	log.Info(lk, "References purged")
}
