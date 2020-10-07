// Copyright 17-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Profits historic data base.
package profitsDb

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/profits"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
	"sort"
	"strconv"
)

var basePath string

func rpath(manager int) string {
	return path.Join(basePath, "Investor-"+strconv.Itoa(manager))
}
func dpath(manager int) string {
	return path.Join(rpath(manager), "profits")
}
func ypath(manager int, year string) string {
	return path.Join(dpath(manager), year+".tb")
}

// Initializes profits historic
//    lk    : Synchronization lock.
//    parent: Parent directory.
func Initialize(lk sync.T, parent string) {
	basePath = parent
	year := date.Now().Format("%Y")
	for i := 0; i < cts.Managers; i++ {
		if !file.Exists(dpath(i)) {
			file.Mkdir(rpath(i))
			file.Mkdir(dpath(i))
			d := date.Now()
			file.WriteAll(ypath(i, year), json.Wa([]json.T{
				profits.New(d.String(), 0, 0, 0).ToJs(),
			}).String())
		}
	}
}

// Read all the investors tables. If an investor has not data,
// the function returns an empty list for it.
//
// Uses the following format:
//   []managerData, where each managerData is
//      []entries, where each entry is a profits.T ordered from before to
//                 after.
//
// Arguments:
//   lk: Synchronization lock.
func Read(lk sync.T) json.T {
	ys := Years(lk)
	sort.Slice(ys, func(i, j int) bool {
		return ys[i] < ys[j]
	})
	var all []json.T
	for i := 0; i < cts.Managers; i++ {
		var managerData []json.T
		for _, y := range ys {
			p := ypath(i, y)
			if file.Exists(p) {
				for _, e := range json.FromString(file.ReadAll(p)).Ra() {
					managerData = append(managerData, e)
				}
			}
		}
		all = append(all, json.Wa(managerData))
	}
	return json.Wa(all)
}

// Returns the list of years with data of any investor. This list is
// unsorted.
//   lk: Synchronization lock.
func Years(lk sync.T) (r []string) {
	for i := 0; i < cts.Managers; i++ {
		for _, finfo := range file.List(dpath(i)) {
			f := finfo.Name()
			y := f[:len(f)-3]
			new := true
			for _, e := range r {
				if e == y {
					new = false
				}
			}
			if new {
				r = append(r, y)
			}
		}
	}
	return
}

// Writes the last entry of an investor.
//
//   lk     : Synchronization lock.
//   manager: Inversor number.
//   total  : Total profits.
//   acc    : Accounting profits.
//   risk   : Risk profits.
func Add(lk sync.T, manager int, total, acc, risk float64) {
	d := date.Now()
	ds := d.String()
	y := d.Format("%Y")
	p := ypath(manager, y)
	newAnn := profits.New(ds, total, acc, risk).ToJs()
	if file.Exists(p) {
		anns := json.FromString(file.ReadAll(p)).Ra()
		l1 := len(anns) - 1
		if profits.FromJs(anns[l1]).Date() == ds {
			anns = append(anns[:l1], newAnn)
		} else {
			anns = append(anns, newAnn)
		}
		file.WriteAll(p, json.Wa(anns).String())
	} else {
		file.WriteAll(p, json.Wa([]json.T{newAnn}).String())
	}
}
