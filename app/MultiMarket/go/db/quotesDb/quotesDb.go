// Copyright 21-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Quotes data base.
package quotesDb

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/matrix"
	"github.com/dedeme/MultiMarket/data/nick"
	"github.com/dedeme/MultiMarket/data/qtable"
	"github.com/dedeme/MultiMarket/data/quote"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"path"
	"sort"
	"strings"
)

var fpath string

// Auxliar function
func nickPath(nk string) string {
	return path.Join(fpath, nk+".tb")
}

// Auxiliar function
func mkQtable(lk sync.T, fnGetValue func(*quote.T) float64) *qtable.T {
	var nks []string
	for _, e := range nicksTb.SelectedNicks(lk) {
		nks = append(nks, e.Name())
	}
	ncols := len(nks)
	nrows := cts.HistoricQuotes
	mx := matrix.New2(nrows, ncols)

	for i, nk := range nks {
		qs := Read(lk, nk)
		if len(qs) == 0 {
			panic("Quotes of nick" + nk + " wrong or not found")
		}
		j := len(qs) - 1
		for _, q := range qs {
			mx[j][i] = fnGetValue(q)
			j--
		}
	}

	return qtable.New(nks, mx)
}

// Initializes quotes data base.
//    parent: Parent directory.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "quotes")
	if !file.Exists(fpath) {
		file.Mkdir(fpath)
	}
}

// Reads quotes of a company.
//
// If an error happens the returned list is empty.
//
// The order of quotes is from after to before.
//    lk      : Synchronization lock.
//    nickName: Nick.
func Read(lk sync.T, nickName string) (ls []*quote.T) {
	p := nickPath(nickName)
	if !file.Exists(p) {
		return
	}
	file.Lines(p, func(s string) bool {
		q, ok := quote.FromString(s)
		if ok {
			ls = append(ls, q)
		}
		return false
	})
	if len(ls) != cts.HistoricQuotes {
		ls = []*quote.T{}
	}
	return
}

// Returns company model quotes.
//
// If an error happens the returned list is empty.
//
// The order of quotes is from after to before.
//    lk: Synchronization lock.
func ReadModel(lk sync.T) (ls []*quote.T) {
	nk, ok := nicksTb.GetModel(lk)
	if ok {
		ls = Read(lk, nk.Name())
	}
	return
}

// Set quotes of a company. If operation fails returns 'false'.
//    lk    : Synchronization lock.
//    nickId: Nick identifier.
//    qs    : Quotes to update.
func SetQuotes(lk sync.T, nickId int, qs []*quote.T) (ok bool) {
	nk, ok := nicksTb.GetNick(lk, nickId)
	if ok {
		if len(qs) != cts.HistoricQuotes {
			log.Error(lk, fmt.Sprintf(
				"%v: Wrong quotes number.\n  Expected: %v\n    Actual: %v",
				nk.Name(), cts.HistoricQuotes, len(qs),
			))
			ok = false
			return
		}
		var qss []string
		for _, e := range qs {
			qss = append(qss, e.String())
		}
		file.WriteAll(nickPath(nk.Name()), strings.Join(qss, "\n"))
		ok = true
		return
	}
	log.Error(lk, fmt.Sprintf("Nick with id '%v' not found", nickId))
	return
}

// Returns a qtable of opens (from before to after) of the selected companies.
//    lk: Synchronization lock.
func Opens(lk sync.T) *qtable.T {
	return mkQtable(lk, func(q *quote.T) float64 { return q.Open() })
}

// Returns a qtable of closes (from before to after) of the selected companies.
//    lk: Synchronization lock.
func Closes(lk sync.T) *qtable.T {
	return mkQtable(lk, func(q *quote.T) float64 { return q.Close() })
}

// Returns a 'map[nick]volume' with the 'cts.QuotesVolume' days volume average
// of every nick in "nicks".
//    lk   : Synchronization lock.
//    nicks: Nick list.
func Volumes(lk sync.T, nicks []*nick.T) (r map[string]float64) {
	r = map[string]float64{}
	for _, nk := range nicks {
		nkName := nk.Name()
		lst := Read(lk, nkName)
		if len(lst) > cts.QuotesVolume {
			lst = lst[:cts.QuotesVolume]
		}
		var vlst []float64
		for _, q := range lst {
			mx := q.Max()
			mn := q.Min()
			v := float64(q.Vol())
			if mx >= 0 && mn >= 0 && v >= 0 {
				vlst = append(vlst, (mx+mn)*v/2)
			}
		}
		sort.Slice(vlst, func(i, j int) bool {
			return vlst[i] < vlst[j]
		})

		var sm float64
		n := 0
		lim := len(vlst) / 2
		for i, v := range vlst {
			if i >= lim {
				break
			}
			sm += v
			n++
		}
		var av float64
		if n > 0 {
			av = sm / float64(n)
		}
		r[nkName] = av
	}
	return
}

// Returns dates of nick model from before to after. Its length is
// 'cts.historicQuotes'. If fails, returns an empty list.
//    lk: Synchronization lock.
func Dates(lk sync.T) (r []string) {
	qs := ReadModel(lk)
	for i := len(qs) - 1; i >= 0; i-- {
		r = append(r, qs[i].Date())
	}
	return
}

// Adds a nick. If nick already exists, logs an error and returns "false".
//    lk      : Synchronization lock.
//    nickName: Nick name to add.
func AddNick(lk sync.T, nickName string) bool {
	p := nickPath(nickName)
	if file.Exists(p) {
		log.Error(lk, "File "+p+" already exists")
		return false
	}
	file.WriteAll(p, "")
	return true
}

// Removes a nick.
//    lk      : Synchronization lock.
//    nickName: Nick name to remove.
func DelNick(lk sync.T, nickName string) {
	file.Remove(nickPath(nickName))
}

// Modifies a nick name.
//
// If "newName" already exists or "oldName" does not exists, it does nothing.
//    lk     : Synchronization lock.
//    oldName: Name to modify.
//    newName: New name.
func ModifyNickName(lk sync.T, oldName, newName string) {
	op := nickPath(oldName)
	np := nickPath(newName)
	if file.Exists(op) && !file.Exists(np) {
		file.Rename(op, np)
	}
}

// Checks quotes in text format.
//    lk  : Synchronization lock.
//    qsTx: Quotes in text format.
//    ----
//    Returns:
//      qs          : Quotes, corrected if necessary.
//      withWarnings: 'true' if some quotes were corrected.
//      withErrors  : 'true' if an error happend. In this case "qs" = [].
func Check(
	lk sync.T, qsTx string,
) (qs []*quote.T, withWarnings, withErrors bool) {
	for _, e := range strings.Split(qsTx, "\n") {
		q, ok := quote.FromString(e)
		if !ok {
			log.Error(lk, "Wrong quote: "+e)
			withErrors = true
			continue
		}
		qs = append(qs, q)
	}
	if withErrors {
		qs = []*quote.T{}
		return
	}
	mqs := ReadModel(lk)
	if len(mqs) == 0 {
		log.Error(lk, "Model quotes can not be read")
		withErrors = true
		qs = []*quote.T{}
		return
	}
	qs, serrors := quote.CorrecDates(mqs, qs)
	qs, serrors2 := quote.Correct(qs)
	serrors = append(serrors, serrors2...)
	if len(serrors) == 0 {
		return
	}
	log.Error(lk, strings.Join(serrors, "\n"))
	withWarnings = true
	return
}

// Checks quotes of a company.
//    lk      : Synchronization lock.
//    nickName: Company nick name.
//    ----
//    Returns:
//      qs          : Quotes, corrected if necessary.
//      withWarnings: 'true' if some quotes were corrected.
//      withErrors  : 'true' if an error happend. In this case "qs" = [].
func CheckQs(
	lk sync.T, nickName string,
) (qs []*quote.T, withWarnings, withErrors bool) {
	p := nickPath(nickName)
	if !file.Exists(p) {
		log.Error(lk, "Quotes file of "+nickName+" not found.")
		withErrors = true
		return
	}
	qsTx := file.ReadAll(p)
	if qsTx == "" {
		log.Error(lk, "Quotes file of "+nickName+" is empty.")
		withErrors = true
		return
	}
	qs, withWarnings, withErrors = Check(lk, qsTx)
	return
}
