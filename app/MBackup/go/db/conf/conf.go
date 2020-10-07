// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings data base.
package conf

import (
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

const (
	langKey = "lang"
)

var fpath string

// Auxiliar function
func write(cf map[string]json.T) {
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
func setField(key string, js json.T) {
	cf := read()
	cf[key] = js
	write(cf)
}

// Intializes table.
//    parent: Parent directory of "Conf.tb".
func Initialize(parent string) {
	fpath = path.Join(parent, "Conf.tb")
	if !file.Exists(fpath) {
		write(map[string]json.T{
			langKey: json.Ws("es"),
		})
	}
}

// Gets web language.
func Lang() string {
	return getField(langKey).Rs()
}

// Sets web language.
//    lang: 'es' or 'en'.
func SetLang(lang string) {
	setField(langKey, json.Ws(lang))
}
