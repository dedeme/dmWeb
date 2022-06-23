// Copyright 08-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Profits historic data base.
package profitsDb

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/profitsEntry"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/math"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/time"
)

var basePath string

func rpath(investor int) string {
	return path.Cat(basePath, "Investor-"+str.Fmt("%d", investor))
}
func dpath(investor int) string {
	return path.Cat(rpath(investor), "profits")
}
func ypath(investor int, year string) string {
	return path.Cat(dpath(investor), year+".tb")
}

// Initializes profits historic
//    parent: Parent directory.
func Initialize(parent string) {
	basePath = parent
	year := time.Fmt("%Y", time.Now())
	for i := 0; i < cts.Investors; i++ {
		if !file.Exists(dpath(i)) {
			file.Mkdir(rpath(i))
			file.Mkdir(dpath(i))
			d := time.Now()
			file.Write(ypath(i, year), js.Wa([]string{
				profitsEntry.New(time.ToStr(d), 0, 0, 0).ToJs(),
			}))
		}
	}
}

// Read all the investors tables. If an investor has not data,
// the function returns an empty list for it.
//
// Uses the following format:
//   []investorData, where each investorData is
//      []entries, where each entry is a profitsEntry.T ordered from before to
//                 after.
func ReadJs() string {
	ys := Years()
	arr.Sort(ys, func(y1, y2 string) bool {
		return y1 < y2
	})

	var all []string
	for i := 0; i < cts.Investors; i++ {
		var investorData []string
		for _, y := range ys {
			p := ypath(i, y)
			if file.Exists(p) {
				for _, e := range js.Ra(str.Trim(file.Read(p))) {
					investorData = append(investorData, e)
				}
			}
		}

		all = append(all, js.Wa(investorData))
	}
	return js.Wa(all)
}

// Returns the list of years with data of investors. This list is
// unsorted.
func Years() (r []string) {
	for i := 0; i < cts.Investors; i++ {
		for _, f := range file.Dir(dpath(i)) {
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
//   investor: Inversor number.
//   total   : Total profits.
//   acc     : Accounting profits.
//   risk    : Risk profits.
func Add(investor int, total, acc, risk float64) {
	rd := func(n float64) float64 {
		return math.Round(n, 2)
	}
	d := time.Now()
	ds := time.ToStr(d)
	y := time.Fmt("%Y", d)
	p := ypath(investor, y)
	newAnn := profitsEntry.New(ds, rd(total), rd(acc), rd(risk)).ToJs()
	if file.Exists(p) {
		anns := js.Ra(file.Read(p))
		l1 := len(anns) - 1
		if profitsEntry.FromJs(anns[l1]).Date() == ds {
			anns[l1] = newAnn
		} else {
			anns = append(anns, newAnn)
		}
		file.Write(p, js.Wa(anns))
	} else {
		file.Write(p, js.Wa([]string{newAnn}))
	}
}
