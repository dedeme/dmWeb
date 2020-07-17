// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Configuration table
package conf

import (
	"github.com/dedeme/News/data/cts"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

func confPath() string {
  return path.Join(cts.WEB_DATA, "Conf.tb")
}

func Init () {
	dir := path.Join(cts.WEB_DATA)
	if !file.Exists(dir) {
		file.Mkdir(dir)
	}
	if !file.Exists(confPath()) {
		file.WriteAll(
      confPath(),
      json.Wo(map[string]json.T{"lang": json.Ws("es")}).String(),
    )
	}
}

// Reads Configuration.
func Read() json.T {
	return json.FromString(file.ReadAll(confPath()))
}

// Sets language.
func SetLang(lang string) {
	o := Read().Ro()
	o["lang"] = json.Ws(lang)
	file.WriteAll(confPath(), json.Wo(o).String())
}
