// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Hconta diary reader.
package hconta

import (
	"github.com/dedeme/CashFlow/data/cash"
	"github.com/dedeme/CashFlow/data/cts"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
)

type entry struct {
	date    string
	desc    string
	debits  map[string]float64
	credits map[string]float64
}

func entryFromJs(json string) *entry {
	a := js.Ra(json)
	dbs := map[string]float64{}
	for k, v := range js.Ro(a[2]) {
		dbs[k] = js.Rd(v)
	}
	cds := map[string]float64{}
	for k, v := range js.Ro(a[3]) {
		cds[k] = js.Rd(v)
	}

	return &entry{
		js.Rs(a[0]),
		js.Rs(a[1]),
		dbs,
		cds,
	}
}

func readCash(e *entry) (ok bool, centry *cash.Entry) {
	sum := 0.0
	for k, v := range e.debits {
		if k == cts.CashAcc {
			sum += v
		}
	}
	for k, v := range e.credits {
		if k == cts.CashAcc {
			sum -= v
		}
	}

	if sum != 0.0 {
		ok = true
		month := e.date[4:6]
		if sum > 0.0 {
			centry = cash.NewEntry(month, e.desc, true, sum)
		} else {
			centry = cash.NewEntry(month, e.desc, false, -sum)
		}
	}
	return
}

// 'balance' is the first cash entry of Hconta diary, and 'diary' are the rest.
func Read(year string) (balance float64, c cash.T) {
	p := path.Cat(cts.HcontaPath, year+".db")
	if !file.Exists(p) {
		return
	}
	allJs := file.Read(p)
	a := js.Ra(allJs)
	first := true
	for _, eJs := range js.Ra(a[3]) {
		if ok, e := readCash(entryFromJs(eJs)); ok {
			if first {
				balance = e.Amount()
				first = false
			} else {
				c = append(c, e)
			}
		}
	}
	return
}
