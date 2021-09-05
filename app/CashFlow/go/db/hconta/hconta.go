// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Hconta diary reader.
package hconta

import (
	"github.com/dedeme/CashFlow/data/cash"
	"github.com/dedeme/CashFlow/data/cts"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

type entry struct {
	date    string
	desc    string
	debits  map[string]float64
	credits map[string]float64
}

func entryFromJs(js json.T) *entry {
	a := js.Ra()
	dbs := map[string]float64{}
	for k, v := range a[2].Ro() {
		dbs[k] = v.Rd()
	}
	cds := map[string]float64{}
	for k, v := range a[3].Ro() {
		cds[k] = v.Rd()
	}

	return &entry{
		a[0].Rs(),
		a[1].Rs(),
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
	p := path.Join(cts.HcontaPath, year+".db")
	if !file.Exists(p) {
		return
	}
	allJs := json.FromString(file.ReadAll(p))
	a := allJs.Ra()
	first := true
	for _, eJs := range a[3].Ra() {
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
