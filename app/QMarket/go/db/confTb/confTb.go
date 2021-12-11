// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global state data base.
package confTb

import (
	"github.com/dedeme/QMarket/data/activity"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

const (
	langKey     = "lang"
	activityKey = "activity"
  ranges = "ranges"
)

var fpath string

// Intializes table.
//    parent: Parent directory of "Conf.tb".
func Initialize(parent string) {
	fpath = path.Join(parent, "Conf.tb")

  var rs []json.T
  for i := 0; i < cts.Qlevels; i++ {
    rs = append(rs, json.Wa([]json.T{}))
  }

	if !file.Exists(fpath) {
		Write(map[string]json.T{
			langKey:     json.Ws("es"),
			activityKey: activity.NewNow(cts.ActSleeping2).ToJs(),
      ranges:      json.Wa(rs),
		})
	}

  conf := Read()
  if len(conf[ranges].Ra()) != cts.Qlevels {
    conf[ranges] = json.Wa(rs)
    Write(conf)
  }

}

// Reads table.
func Read() map[string]json.T {
	return json.FromString(file.ReadAll(fpath)).Ro()
}

// Writes table.
func Write(cf map[string]json.T) {
	file.WriteAll(fpath, json.Wo(cf).String())
}

// Gets web language.
func Lang() (lang string) {
	return Read()[langKey].Rs()
}

// Sets web language.
//    lang: 'es' or 'en'.
func SetLang(lang string) {
	conf := Read()
	conf[langKey] = json.Ws(lang)
	Write(conf)
}

// Gets current activity.
func Activity() (ac *activity.T) {
	return activity.FromJs(Read()[activityKey])
}

// Sets current activity.
//    act : One of cts.ActXXX.
func SetActivity(ac *activity.T) {
	conf := Read()
	conf[activityKey] = ac.ToJs()
	Write(conf)
}

// Gets current range of an investor (Qlevel)
func Range (inv int) []int {
  var r []int
  for _, ijs := range Read()[ranges].Ra()[inv].Ra() {
    r = append(r, ijs.Ri())
  }
  return r
}

// Sets current range of an investor (Qlevel)
func SetRange (inv int, irange []int) {
  var r []json.T
  for _, i := range irange {
    r = append(r, json.Wi(i))
  }

  conf := Read()
  a := conf[ranges].Ra()
  a[inv] = json.Wa(r)
  conf[ranges] = json.Wa(a)
  Write(conf)
}
