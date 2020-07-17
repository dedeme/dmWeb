// Copyright 10-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Authors evaluation table.
package eauthors

import (
	"github.com/dedeme/News/data/cts"
	"github.com/dedeme/News/data/eval"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

func eauthorsPath() string {
	return path.Join(cts.WEB_DATA, "Eauthors.tb")
}

// Initializes table.
func Init() {
	if !file.Exists(eauthorsPath()) {
		file.WriteAll(eauthorsPath(), "[]")
	}
}

// Reads table.
func Read() (r []*eval.T) {
  for _, e := range json.FromString(file.ReadAll(eauthorsPath())).Ra() {
    r = append(r, eval.FromJs(e))
  }
	return
}

// Writes table
func Write(es []*eval.T) {
  var a []json.T
  for _, e := range es {
    a = append(a, e.ToJs())
  }
  file.WriteAll(eauthorsPath(), json.Wa(a).String())
}
