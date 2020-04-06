// Copyright 02-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Personal libraries data base
package plibs

import (
  "github.com/dedeme/golib/file"
  "github.com/dedeme/golib/json"
  "github.com/dedeme/GoDoc/data/plibrow"
)

var ppath string
var spath string

// Libraries type enum: Personal | Standard
type LibsT int
const (
  Personal LibsT = iota
  Standard
)

// Initializes libraries
func Initialize (personalPath, standardPath  string) {
  ppath = personalPath
  if !file.Exists(ppath) {
    write(Personal, json.Wa([]json.T{}))
  }
  spath = standardPath
  if !file.Exists(spath) {
    write(Standard, json.Wa([]json.T{}))
  }
}

func write (libs LibsT, js json.T) {
  path := ppath
  if libs == Standard {
    path = spath
  }
  file.WriteAll(path, js.String())
}

// Returns the complete sorted library.
func Read (libs LibsT) json.T {
  path := ppath
  if libs == Standard {
    path = spath
  }
  return json.FromString(file.ReadAll(path))
}

// Promotes 'lib' after click.
func Update (libs LibsT, lib string) {
  ljs := Read(libs)
  l := plibrow.Update(plibrow.FromJs(ljs), lib)
  write(libs, plibrow.ToJs(l))
}

// Renoves every library.
func UpdateAll(libs LibsT, allLibs []string) {
  ljs := Read(libs)
  l := plibrow.UpdateAll(plibrow.FromJs(ljs), allLibs);
  write(libs, plibrow.ToJs(l))
}
