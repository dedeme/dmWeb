// Copyright 10-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Words evaluation data base
package ewords

import (
	"github.com/dedeme/News/data/cts"
	"github.com/dedeme/News/data/eval"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

func dbPath() string{
  return path.Join(cts.WEB_DATA, "words")
}

func letterPath(letter string) string {
  return path.Join(dbPath(), letter)
}

func ewordsPath(letter string) string {
	return path.Join(letterPath(letter), "Words.tb")
}

// Initializes words data base.
func Init() {
	if !file.Exists(dbPath()) {
		file.Mkdir(dbPath())
	}
}

// Returns letters of data base. (0 is for any number)
func ReadLetters() []string {
  var r []string
  for _, e := range file.List(dbPath()) {
    r = append(r, e.Name())
  }
  return r
}

// Returns table evaluations of a letter.
func Read(letter string) (r []*eval.T) {
  if !file.Exists(ewordsPath(letter)){
    return
  }
  for _, e := range json.FromString(file.ReadAll(ewordsPath(letter))).Ra() {
    r = append(r, eval.FromJs(e))
  }
	return
}

// Writes table evaluations of a letter.
func Write(letter string, es []*eval.T) {
  if !file.Exists(letterPath(letter)) {
    file.Mkdir(letterPath(letter))
  }
  var a []json.T
  for _, e := range es {
    a = append(a, e.ToJs())
  }
  file.WriteAll(ewordsPath(letter), json.Wa(a).String())
}

