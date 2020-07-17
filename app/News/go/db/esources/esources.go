// Copyright 10-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Sources evaluation table.
package esources

import (
	"github.com/dedeme/News/data/cts"
	"github.com/dedeme/News/data/eval"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

func esourcesPath() string {
	return path.Join(cts.WEB_DATA, "Esources.tb")
}

// Initializes table.
func Init() {
	if !file.Exists(esourcesPath()) {
		file.WriteAll(esourcesPath(), "[]")
	}
}

// Reads table.
func Read() (r []*eval.T) {
  for _, e := range json.FromString(file.ReadAll(esourcesPath())).Ra() {
    r = append(r, eval.FromJs(e))
  }
	return
}

// Writes table.
func Write(es []*eval.T) {
  var a []json.T
  for _, e := range es {
    a = append(a, e.ToJs())
  }
  file.WriteAll(esourcesPath(), json.Wa(a).String())
}
