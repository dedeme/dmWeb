// Copyright 13-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings data base.
package conf

import (
	"github.com/dedeme/MMarket/data/activity"
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

const (
	langKey     = "lang"
	activityKey = "activity"
)

// Configuration file path.
var fpath string

// Write every configuration entry.
func write(lk sync.T, cf map[string]json.T) {
	file.WriteAll(fpath, json.Wo(cf).String())
}

// Read every configuration entry.
func read() map[string]json.T {
	return json.FromString(file.ReadAll(fpath)).Ro()
}

// Read the entry with key "key"
func getField(lk sync.T, key string) json.T {
	return read()[key]
}

// Write the entry with ket "key"
func setField(lk sync.T, key string, js json.T) {
	cf := read()
	cf[key] = js
	write(lk, cf)
}

// Intializes table.
//    lk  : Synchronization lock.
//    parent: Parent directory of "Conf.tb".
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Conf.tb")
	if !file.Exists(fpath) {
		write(lk, map[string]json.T{
			langKey:     json.Ws("es"),
			activityKey: activity.New(cts.ActSleeping2).ToJs(),
		})
	}
}

// Gets web language.
func Lang(lk sync.T) string {
	return getField(lk, langKey).Rs()
}

// Sets web language.
//    lk  : Synchronization lock.
//    lang: 'es' or 'en'.
func SetLang(lk sync.T, lang string) {
	setField(lk, langKey, json.Ws(lang))
}

// Gets current activity.
func Activity(lk sync.T) *activity.T {
	return activity.FromJs(getField(lk, activityKey))
}

// Sets current activity.
//    lk  : Synchronization lock.
//    act : One of cts.ActXXX.
func SetActivity(lk sync.T, act *activity.T) {
	setField(lk, activityKey, act.ToJs())
}
