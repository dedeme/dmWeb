// Copyright 07-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Models data base.
package modelsDb

import (
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
	"strconv"
)

var fpath string

func fileToGroup(f string) int {
	r, _ := strconv.Atoi(f[:len(f)-3])
	return r
}

func groupToFile(group int) string {
	n := strconv.Itoa(group)
	if len(n) < 2 {
		n = "0" + n
	}
	return n + ".tb"
}

// Auxliar function
func rangePath(group int) string {
	return path.Join(fpath, groupToFile(group))
}

// Initializes and clean models data base.
//    parent: Parent directory.
func Initialize(parent string) {
	fpath = path.Join(parent, "models")
	if !file.Exists(fpath) {
		file.Mkdir(fpath)
	}

	min := cts.RangesMin
	max := cts.RangesMin + cts.RangesGroups
	for _, fi := range file.List(fpath) {
		r := fileToGroup(fi.Name())
		if r < min || r >= max {
			file.Remove(rangePath(r))
		}
	}
}

func Write(group int, models *model.TableT) {
	file.WriteAll(rangePath(group), models.ToJs().String())
}

func Read(group int) (ok bool, models *model.TableT) {
	p := rangePath(group)
	if !file.Exists(p) {
		return
	}
	ok = true
	models = model.TableFromJs(json.FromString(file.ReadAll(p)))
	return
}

// Returns the table group of a model
func Group(md *model.T) int {
	return md.Id() / cts.RangesGroupNumber
}

// Do a function with each element of model results database.
//
// The function is run from the least parameter to the greats one in turn and it
// is stopped when returns 'true' or the paramenters are exausted.
//    fn: Function to execute with each record.
//        fn arguments are:
//          paramId      : Parameter which generated the results.
//          result       : Evaluation results. One for each level (0...Cts.qlevels).
//          RETURN       : true if 'fn' must stop after execute it.
func EachResult(fn func(int, []*model.RsT) bool) {
	stop := false
	for gr := cts.RangesMin; gr < cts.RangesMin+cts.RangesGroups; gr++ {
		ok, tb := Read(gr)
		rss := tb.Results()
		if ok {
			for i := 0; i < cts.RangesGroupNumber; i++ {
				paramId := gr*cts.RangesGroupNumber + i
				if fn(paramId, rss[paramId]) {
					stop = true
					break
				}
			}
		}
		if stop {
			break
		}
	}
}
