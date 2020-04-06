// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package conf

import (
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	gpath "path"
)

var path string

// Initializes conf.db.
//    dir: Parent directory.
func Initialize(dir string) {
	path = gpath.Join(dir, "conf.db")
	if !file.Exists(path) {
		db := map[string]json.T{
			"lang": json.Ws("es"),
		}
		write(db)
	}
}

func write(db map[string]json.T) {
	file.WriteAll(path, json.Wo(db).String())
}

func read() map[string]json.T {
	return json.FromString(file.ReadAll(path)).Ro()
}

func Lang() string {
	js := read()["lang"]
	return js.Rs()
}

func SetLang(lang string) {
	db := read()
	db["lang"] = json.Ws(lang)
	write(db)
}
