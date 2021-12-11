// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Quotes data base.
package quotesDb

import (
	"errors"
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/matrix"
	"github.com/dedeme/QMarket/data/nick"
	"github.com/dedeme/QMarket/data/qtable"
	"github.com/dedeme/QMarket/data/quote"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/golib/file"
	"path"
)

var fpath string

// Auxliar function
func nickPath(nk string) string {
	return path.Join(fpath, nk+".tb")
}

// Auxiliar function
func mkQtable(fnGetValue func(*quote.T) float64) *qtable.T {
	var nks []string
	for _, e := range nicksTb.Read().SelectedNicks() {
		nks = append(nks, e.Name())
	}
	ncols := len(nks)
	nrows := cts.HistoricQuotes
	mx := matrix.New(nrows, ncols)

	for i, nk := range nks {
		qs, err := Read(nk)
		if err != nil {
			panic(err.Error())
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
func Initialize(parent string) {
	fpath = path.Join(parent, "quotes")
	if !file.Exists(fpath) {
		file.Mkdir(fpath)
	}
}

// Returns quotes of a company.
//
// The order of quotes is from after to before.
//
// Each line is a quote in string format or an empty string.
//
//    nickName: Company nick name (TEF, BBVA, ...)
func Read(nickName string) (
	qs []*quote.T, err error,
) {
	p := nickPath(nickName)
	if !file.Exists(p) {
		err = errors.New("Quotes file " + nickName + ".tb not found.")
		return
	}
	qs, err = quote.TxToQs(file.ReadAll(p))
	if err != nil {
		err = errors.New(nickName + ": " + err.Error())
	}
	return
}

// Writes quotes of a company.
//
// It returns 'ok==false' if the nickName file does not exist.
//  nickName: Company nick name (TEF, BBVA, ...)
//  qs: Correct (number and quotes) quotes.
func Write(nickName string, qs []*quote.T) (ok bool) {
	p := nickPath(nickName)
	if !file.Exists(p) {
		return
	}

	tx := ""
	for i, q := range qs {
		if i == 0 {
			tx = q.String()
			continue
		}
		tx += "\n" + q.String()
	}
	file.WriteAll(p, tx)
	ok = true
	return
}

// Returns true if table exists
func Exists(nickName string) bool {
	return file.Exists(nickPath(nickName))
}

// Remove a table
func Delete(nickName string) {
	file.Remove(nickPath(nickName))
}

// Returns a qtable of opens (from before to after) of the selected companies.
func Opens() *qtable.T {
	return mkQtable(func(q *quote.T) float64 { return q.Open() })
}

// Returns a qtable of closes (from before to after) of the selected companies.
func Closes() *qtable.T {
	return mkQtable(func(q *quote.T) float64 { return q.Close() })
}

// Returns dates of nick model from before to after. Its length is
// 'cts.historicQuotes'.
//    lk: Synchronization lock.
func Dates() (r []string) {
	var nk *nick.T
	nksTb := nicksTb.Read()
	nkId, ok := nksTb.Model()
	if ok {
		nk, ok = nksTb.NickFromId(nkId)
	}
	if !ok {
		nk = nksTb.List()[0]
	}
	qs, _ := Read(nk.Name())
	for i := len(qs) - 1; i >= 0; i-- {
		r = append(r, qs[i].Date())
	}
	return
}

// Modifies a nick name.
//
// If "newName" already exists or "oldName" does not exists, it does nothing.
//    oldName: Name to modify.
//    newName: New name.
func ModifyNickName(oldName, newName string) {
	op := nickPath(oldName)
	np := nickPath(newName)
	if file.Exists(op) && !file.Exists(np) {
		file.Rename(op, np)
	}
}

// Checks quotes of a company, writting error and warnings in Log.
//    nickName: Nick source (only for reports)
//    qs: Company quotes.
//    ----
//    Returns:
//      newQqs      : Quotes, corrected if necessary.
//      withWarnings: 'true' if some quotes were corrected.
//      withErrors  : 'true' if an error happend. In this case "qs" = [].
func Check(
	nickName string, qs []*quote.T,
) (newQs []*quote.T, withWarnings, withErrors bool) {

	nksTb := nicksTb.Read()
	mdId, ok := nksTb.Model()
	if !ok {
		logTb.Error("Nick model has not been set")
		withErrors = true
		return
	}
	md, ok := nksTb.NickFromId(mdId)
	if !ok {
		logTb.Error(fmt.Sprintf("Nick model with id=%v not found", mdId))
		withErrors = true
		return
	}
	mdQs, err := Read(md.Name())
	if err != nil {
		logTb.Error(err.Error())
		withErrors = true
		return
	}

	newQs, dateErrors := quote.CorrecDates(mdQs, qs)
	for _, e := range dateErrors {
		logTb.Error(fmt.Sprintf("Corrected in %v:\n%v", nickName, e))
		withWarnings = true
	}

	newQs, valueErrors := quote.Correct(newQs)
	for _, e := range valueErrors {
		logTb.Error(fmt.Sprintf("Corrected in %v:\n%v", nickName, e))
		withWarnings = true
	}

	return
}
