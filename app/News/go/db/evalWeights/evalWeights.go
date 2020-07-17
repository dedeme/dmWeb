// Copyright 10-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Evaluation data base
package evalWeights

import (
	"github.com/dedeme/News/data/cts"
	"github.com/dedeme/News/data/evalWeight"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

func evalsPath() string {
	return path.Join(cts.WEB_DATA, "Evals.tb")
}

// Initializes eval data base.
func Init() {
	if !file.Exists(evalsPath()) {
		file.WriteAll(
      evalsPath(),
      json.Wa([]json.T{
        json.Wd(cts.THIRD),
        json.Wd(cts.THIRD),
        json.Wd(cts.THIRD),
      }).String(),
    )
	}
}

// Reads evalutation ponderations
func Read() *evalWeight.T {
	return evalWeight.FromJs(json.FromString(file.ReadAll(evalsPath())))
}

// Writes evaluation ponderations
func Write(e *evalWeight.T) {
  file.WriteAll(evalsPath(), e.ToJs().String())
}
