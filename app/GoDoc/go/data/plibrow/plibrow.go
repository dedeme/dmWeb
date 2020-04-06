// Copyright 01-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Standard library row data
package plibrow

import (
	"github.com/dedeme/GoDoc/data/cts"
	"github.com/dedeme/golib/json"
	"sort"
)

type T struct {
	path  string
	order int
	pond  float64
}

func new(path string) *T {
	return &T{path, 0, 0}
}

type sorter []*T

func (a sorter) Len() int           { return len(a) }
func (a sorter) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a sorter) Less(i, j int) bool { return a[i].order > a[j].order }

func repond(libs []*T) {
	count := 0
	max := -1.0
	for _, l := range libs {
		count++
		if l.pond > max {
			max = l.pond
		}
	}
	if max > float64(count) {
		for _, l := range libs {
			l.pond = l.pond * float64(count) / max
		}
	}
}

func reorder(libs []*T) []*T {
	sort.Sort(sorter(libs))
	v := cts.PersonalMenuEntries
	for _, r := range libs {
		r.order = v
		if v > 0 {
			v--
		}
	}
	return libs
}

// Adds a clicked library to 'libs'.
//
// Depth of lib is indicated by none, one or more "·" at the beginning of
// string.
//
// One asterisk at the end of 'lib' indicates that it is a directory.
func Update(libs []*T, lib string) []*T {
	var row *T
	for _, r := range libs {
		if r.path == lib {
			row = r
			break
		}
	}
	if row == nil {
		row = new(lib)
		libs = append(libs, row)
	}

	row.order = cts.PersonalMenuEntries + 1
	row.pond += 1

  repond(libs)
	return reorder(libs)
}

// Renews every library in 'olds' with 'news'.
//
// Structure of each librarary indicated by 'news' is explained in Update.
func UpdateAll(olds []*T, news []string) []*T {
	var rs []*T
	for _, n := range news {
		nrow := new(n)
		for _, old := range olds {
			if n == old.path {
				nrow.order = old.order
				nrow.pond = old.pond
				break
			}
		}
		rs = append(rs, nrow)
	}
	return reorder(rs)
}

// Converter.
func ToJs(libs []*T) json.T {
	var rs []json.T
	for _, r := range libs {
		rs = append(rs, json.Wa([]json.T{
			json.Ws(r.path),
			json.Wi(r.order),
			json.Wd(r.pond),
		}))
	}
	return json.Wa(rs)
}

// Converter.
func FromJs(js json.T) (rs []*T) {
	a := js.Ra()
	for _, rjs := range a {
		a2 := rjs.Ra()
		rs = append(rs, &T{
			a2[0].Rs(),
			a2[1].Ri(),
			a2[2].Rd(),
		})
	}
	return
}
