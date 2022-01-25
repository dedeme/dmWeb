// Copyright 02-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Investors table.
package investorsTb

import (
	"github.com/dedeme/QMarket/data/investors"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/data/qtable"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initializes table.
//    parent: Parent directory.
func Initialize(parent string) {
	fpath = path.Join(parent, "Investors.tb")
	if !file.Exists(fpath) {
		Write(investors.NewTable())
	}
	Regularize()
}

// Writes table.
func Write(t *investors.TableT) {
	file.WriteAll(fpath, t.ToJs().String())
}

// Reads table
func Read() *investors.TableT {
	return investors.TableFromJs(json.FromString(file.ReadAll(fpath)))
}

// Update investors table keep in mind to normalize company parameters
// toward the base value.
func Regularize() {
	nks := nicksTb.Read().List()
	isToSell := func(nickName string, qlevel, paramId int) bool {
		closesTb := quotesDb.Closes()
		ix := -1
		for i, n := range closesTb.Nicks() {
			if n == nickName {
				ix = i
				break
			}
		}
		if ix == -1 {
			panic("When regularizing in scheduler, nick " + nickName + " not found")
		}
		md := model.New(qlevel, paramId)
		return md.IsToSell(qtable.GetCol(closesTb.Values(), ix))
	}
	itb := Read()
	if itb.Params.Regularize(itb.Base, nks, isToSell) {
		Write(itb)
	}
}
