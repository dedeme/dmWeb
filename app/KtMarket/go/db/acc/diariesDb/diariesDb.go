// Copyright 08-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting diaries data base.
package diariesDb

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/acc"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/time"
)

var basePath string

func rpath(investor int) string {
	return path.Cat(basePath, "Investor-"+str.Fmt("%d", investor))
}
func dpath(investor int) string {
	return path.Cat(rpath(investor), "diaries")
}
func ypath(investor int, year string) string {
	return path.Cat(dpath(investor), year+".tb")
}

// Auxiliar function
func lastYear(investor int) (y string, ok bool) {
	invDir := dpath(investor)
	var tbs []string
	for _, n := range file.Dir(invDir) {
		if str.Ends(n, ".tb") {
			tbs = append(tbs, n)
		}
	}
	if len(tbs) > 0 {
		arr.Sort(tbs, func(s1, s2 string) bool {
			return s1 < s2
		})
		lastTb := tbs[len(tbs)-1]
		y = lastTb[:len(lastTb)-3]
		ok = true
	}
	return
}

// Auxiliar function
func newYearInit(investor int, year string) {
	lastY, ok := lastYear(investor)
	if !ok {
		panic(str.Fmt("Tables of investor %v not found", investor))
	}

	data, ok := Read(investor, lastY)
	if !ok {
		panic(str.Fmt(
			"Table of investor %v, year %v, not found", investor, year,
		))
	}
	operations, serrors := acc.Regularize(data.Annotations)
	if len(serrors) != 0 {
		log.Error(str.Fmt(
			"Errors initializing the new year %v:\n%v",
			year, arr.Join(serrors, "\n"),
		))
	}
	date := year + "0101"
	var annJs []string
	for i, op := range operations {
		annJs = append(annJs, acc.AnnotationToJs(acc.NewAnnotation(i, date, op)))
	}
	file.Write(ypath(investor, year), js.Wa([]string{
		js.Wi(len(operations)),
		js.Wa(annJs),
	}))
}

// Initializes Diaries table.
//    parent: Parent directory.
func Initialize(parent string) {
	basePath = parent
	year := time.Fmt("%Y", time.Now())
	for i := 0; i < cts.Investors; i++ {
		if !file.Exists(dpath(i)) {
			file.Mkdir(rpath(i))
			file.Mkdir(dpath(i))
			file.Write(ypath(i, year), js.Wa([]string{
				js.Wi(0),
				js.Wa([]string{}),
			}))
		}
		if !file.Exists(ypath(i, year)) { // New year
			newYearInit(i, year)
		}
	}
}

// Returns unsorted annotations of "year" and "investor" or 'ok==nil' if data
// is not found.
//    investor: Inversor number.
//    year    : Year to search.
func ReadJs(investor int, year string) (data string, ok bool) {
	p := ypath(investor, year)
	if !file.Exists(p) {
		return
	}
	data = js.Ra(file.Read(p))[1]
	ok = true
	return
}

// Returns unsorted annotations of "year" of every investor.
//    year    : Year to search.
func ReadAllJs(year string) string {
	var r []string
	for i := 0; i < cts.Investors; i++ {
		anns, ok := ReadJs(i, year)
		var annsJs []string
		if ok {
			annsJs = js.Ra(anns)
		}
		r = append(r, annsJs...)
	}
	return js.Wa(r)
}

// Returns table data of "year" and "investor" or 'ok==nil' if the table is not
// found.
//    investor: Inversor number.
//    year    : Year to search.
func Read(investor int, year string) (data *acc.AnnotationTbT, ok bool) {
	p := ypath(investor, year)
	if !file.Exists(p) {
		return
	}
	data = acc.AnnotationTbFromJs(file.Read(p))
	ok = true
	return
}

// Returns unsorted annotations data of last year of an "investor".
//    investor: Inversor number.
func ReadAnnotations(investor int) (r []*acc.AnnotationT) {
	lastY, ok := lastYear(investor)
	if ok {
		data, _ := Read(investor, lastY)
		r = data.Annotations
	}
	return
}

// Writes annotations data of "year" and "investor"
//    investor: Inversor number.
//    year    : Year to search.
//    data    : File data.
func Write(investor int, year string, data *acc.AnnotationTbT) {
	file.Write(ypath(investor, year), acc.AnnotationTbToJs(data))
}

// Returns all the years with annotations, sorted from after to before.
func Years() (r []string) {
	rmap := map[string]bool{}
	for i := 0; i < cts.Investors; i++ {
		for _, nm := range file.Dir(dpath(i)) {
			if str.Ends(nm, ".tb") {
				rmap[nm[:len(nm)-3]] = true
			}
		}
	}
	for k := range rmap {
		r = append(r, k)
	}
	arr.Sort(r, func(s1, s2 string) bool {
		return s1 > s2
	})
	return
}

// Returns the sum of cash of all the investors for a "year".
//    year: Year to search.
func CashAll(year string) (r float64, serrors []string) {
	for i := 0; i < cts.Investors; i++ {
		data, ok := Read(i, year)
		if ok {
			ledger, _, _, serrs := acc.Settlement(data.Annotations)
			r += ledger.Cash
			serrors = append(serrors, serrs...)
		}
	}
	return
}

// Returns the sum of cash of all the investors of last year until 'd'.
//    d : Final date inclusive.
func CashAllUpTo(d string) (r float64, serrors []string) {
	for i := 0; i < cts.Investors; i++ {
		data, ok := Read(i, Years()[0])
		var anns []*acc.AnnotationT
		for _, a := range data.Annotations {
			if a.Date <= d {
				anns = append(anns, a)
			}
		}
		if ok {
			ledger, _, _, serrs := acc.Settlement(anns)
			r += ledger.Cash
			serrors = append(serrors, serrs...)
		}
	}
	return
}

// Deletes one annotation.
//    investor: Inversor number.
//    year    : Year to search.
//    annId   : Annotation identifier.
func Del(investor int, year string, annId int) {
	data, ok := Read(investor, year)
	if ok {
		var newAnns []*acc.AnnotationT
		for _, e := range data.Annotations {
			if e.Id != annId {
				newAnns = append(newAnns, e)
			}
		}
		Write(investor, year, acc.NewAnnotationTb(data.NextId, newAnns))
	}
}

// Adds one annotation.
//    investor: Inversor number.
//    year    : Year to search.
//    ann     : Annotation to add (its Id() will be changed).
func Add(investor int, year string, ann *acc.AnnotationT) {
	data, ok := Read(investor, year)
	if !ok {
		log.Error(str.Fmt("Table %v not found.", ypath(investor, year)))
		return
	}
	ann = acc.NewAnnotation(data.NextId, ann.Date, ann.Operation)
	newAnns := append(data.Annotations, ann)
	Write(investor, year, acc.NewAnnotationTb(data.NextId+1, newAnns))
}
