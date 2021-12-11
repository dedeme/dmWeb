// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting diaries data base.
package diariesDb

import (
	"fmt"
	"github.com/dedeme/QMarket/data/acc"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
	"sort"
	"strconv"
	"strings"
)

type T struct {
	nextId      int
	annotations []*acc.AnnotationT
}

func (tb *T) Annotations() []*acc.AnnotationT {
	return tb.annotations
}

var basePath string

func rpath(investor int) string {
	return path.Join(basePath, "Investor-"+strconv.Itoa(investor))
}
func dpath(investor int) string {
	return path.Join(rpath(investor), "diaries")
}
func ypath(investor int, year string) string {
	return path.Join(dpath(investor), year+".tb")
}

// Auxiliar function
func lastYear(investor int) (y string, ok bool) {
	invDir := dpath(investor)
	var tbs []string
	for _, e := range file.List(invDir) {
		n := e.Name()
		if strings.HasSuffix(n, ".tb") {
			tbs = append(tbs, n)
		}
	}
	if len(tbs) > 0 {
		sort.Strings(tbs)
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
		panic(fmt.Sprintf("Tables of investor %v not found", investor))
	}

	data, ok := Read(investor, lastY)
	if !ok {
		panic(fmt.Sprintf(
			"Table of investor %v, year %v, not found", investor, year,
		))
	}
	operations, serrors := acc.Regularize(data.annotations)
	if len(serrors) != 0 {
		logTb.Error(fmt.Sprintf(
			"Errors initializing the new year %v:\n%v",
			year, strings.Join(serrors, "\n"),
		))
	}
	date := year + "0101"
	var annJs []json.T
	for i, op := range operations {
		annJs = append(annJs, acc.NewAnnotation(i, date, op).ToJs())
	}
	file.WriteAll(ypath(investor, year), json.Wa([]json.T{
		json.Wi(len(operations)),
		json.Wa(annJs),
	}).String())
}

// Initializes calendar table.
//    parent: Parent directory.
func Initialize(parent string) {
	basePath = parent
	year := date.Now().Format("%Y")
	for i := 0; i < cts.Qlevels; i++ {
		if !file.Exists(dpath(i)) {
			file.Mkdir(rpath(i))
			file.Mkdir(dpath(i))
			file.WriteAll(ypath(i, year), json.Wa([]json.T{
				json.Wi(0),
				json.Wa([]json.T{}),
			}).String())
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
func ReadJs(investor int, year string) (data json.T, ok bool) {
	p := ypath(investor, year)
	if !file.Exists(p) {
		return
	}
	data = json.FromString(file.ReadAll(p)).Ra()[1]
	ok = true
	return
}

// Returns unsorted annotations of "year" of every investor.
//    year    : Year to search.
func ReadAllJs(year string) json.T {
	var r []json.T
	for i := 0; i < cts.Qlevels; i++ {
		anns, ok := ReadJs(i, year)
		var annsJs []json.T
		if ok {
			annsJs = anns.Ra()
		}
		r = append(r, annsJs...)
	}
	return json.Wa(r)
}

// Returns table data of "year" and "investor" or 'ok==nil' if the table is not
// found.
//    investor: Inversor number.
//    year    : Year to search.
func Read(investor int, year string) (data *T, ok bool) {
	p := ypath(investor, year)
	if !file.Exists(p) {
		return
	}
	a := json.FromString(file.ReadAll(p)).Ra()
	nextId := a[0].Ri()
	var anns []*acc.AnnotationT
	for _, e := range a[1].Ra() {
		anns = append(anns, acc.AnnotationFromJs(e))
	}
	data = &T{nextId, anns}
	ok = true
	return
}

// Returns unsorted annotations data of last year of an "investor".
//    investor: Inversor number.
func ReadAnnotations(investor int) (r []*acc.AnnotationT) {
	lastY, ok := lastYear(investor)
	if ok {
		data, _ := Read(investor, lastY)
		r = data.annotations
	}
	return
}

// Writes annotations data of "year" and "investor"
//    investor: Inversor number.
//    year    : Year to search.
//    data    : File data.
func Write(investor int, year string, data *T) {
	var annJs []json.T
	for _, e := range data.annotations {
		annJs = append(annJs, e.ToJs())
	}
	file.WriteAll(ypath(investor, year), json.Wa([]json.T{
		json.Wi(data.nextId),
		json.Wa(annJs),
	}).String())
}

// Returns all the years with annotations, sorted from after to before.
func Years() (r []string) {
	rmap := map[string]bool{}
	for i := 0; i < cts.Qlevels; i++ {
		for _, e := range file.List(dpath(i)) {
			nm := e.Name()
			if strings.HasSuffix(nm, ".tb") {
				rmap[nm[:len(nm)-3]] = true
			}
		}
	}
	for k := range rmap {
		r = append(r, k)
	}
	sort.Slice(r, func(i, j int) bool {
		return r[i] > r[j]
	})
	return
}

// Returns the sum of cash of all the investors for a "year".
//    year: Year to search.
func CashAll(year string) (r float64, serrors []string) {
	for i := 0; i < cts.Qlevels; i++ {
		data, ok := Read(i, year)
		if ok {
			ledger, _, serrs := acc.Settlement(data.annotations)
			r += ledger.Cash()
			serrors = append(serrors, serrs...)
		}
	}
	return
}

// Returns the sum of cash of all the investors of last year until 'd'.
//    d : Final date inclusive.
func CashAllUpTo(d string) (r float64, serrors []string) {
	for i := 0; i < cts.Qlevels; i++ {
		data, ok := Read(i, Years()[0])
		var anns []*acc.AnnotationT
		for _, a := range data.annotations {
			if a.Date() <= d {
				anns = append(anns, a)
			}
		}
		if ok {
			ledger, _, serrs := acc.Settlement(anns)
			r += ledger.Cash()
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
		for _, e := range data.annotations {
			if e.Id() != annId {
				newAnns = append(newAnns, e)
			}
		}
		Write(investor, year, &T{data.nextId, newAnns})
	}
}

// Adds one annotation.
//    investor: Inversor number.
//    year    : Year to search.
//    ann     : Annotation to add (its Id() will be changed).
func Add(investor int, year string, ann *acc.AnnotationT) {
	data, ok := Read(investor, year)
	if !ok {
		logTb.Error(fmt.Sprintf("Table %v not found.", ypath(investor, year)))
		return
	}
	ann = acc.NewAnnotation(data.nextId, ann.Date(), ann.Operation())
	newAnns := append(data.annotations, ann)
	Write(investor, year, &T{data.nextId + 1, newAnns})
}
