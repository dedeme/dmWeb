// Copyright 16-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings data base.
package conf

import (
	"github.com/dedeme/MultiMarket/data/activity"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

const (
	langKey     = "lang"
	activityKey = "activity"
)

var fpath string

// Auxiliar function
func write(lk sync.T, cf map[string]json.T) {
	file.WriteAll(fpath, json.Wo(cf).String())
}

// Auxiliar function
func read() map[string]json.T {
	return json.FromString(file.ReadAll(fpath)).Ro()
}

// Auxiliar function
func getField(key string) json.T {
	return read()[key]
}

// Auxiliar function
func setField(lk sync.T, key string, js json.T) {
	cf := read()
	cf[key] = js
	write(lk, cf)
}

// Intializes table.
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
func Lang() string {
	return getField(langKey).Rs()
}

// Sets web language.
//    lk  : Synchronization lock.
//    lang: 'es' or 'en'.
func SetLang(lk sync.T, lang string) {
	setField(lk, langKey, json.Ws(lang))
}

// Gets current activity.
func Activity(lk sync.T) *activity.T {
	return activity.FromJs(getField(activityKey))
}

// Sets current activity.
//    lk  : Synchronization lock.
//    act : One of cts.ActXXX.
func SetActivity(lk sync.T, act *activity.T) {
	setField(lk, activityKey, act.ToJs())
}
