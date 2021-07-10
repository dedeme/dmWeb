// Copyright 04-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Results of fleas models with only one parameter.
package resultsDb

import (
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/flea/paramEval"
	"github.com/dedeme/MultiMarket/data/flea/result"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"io"
	"os"
	"path"
)

var parentDir string

func filePath(modelId string) string {
	return path.Join(parentDir, modelId+".tb")
}

func tmpFilePath() string {
	return path.Join(parentDir, "tmp.tb")
}

func daysDatePath() string {
	return path.Join(parentDir, "DaysDate.tb")
}

func Initialize(lk sync.T, parent string) {
  parentDir = parent
	if !file.Exists(parentDir) {
		file.Mkdirs(parentDir)
		file.WriteAll(daysDatePath(), json.Wo(map[string]json.T{}).String())
	}
	for _, md := range fmodels.List() {
		if len(md.ParNames()) == 1 {
			if !file.Exists(filePath(md.Id())) {
				file.OpenWrite(filePath(md.Id())).Close()
				WriteDaysDate(lk, md.Id(), 0, "20010101")
			}
		}
	}
}

// Writes a model days and date.
//    lk     : Synchronization lock.
//    modelId: Model identifier.
//    days   : Evaluated days to calculate movil average.
//    date   : Last date evaluated
func WriteDaysDate(lk sync.T, modelId string, days int, date string) {
	data := ReadDaysDate(lk)
	data[modelId] = []json.T{json.Wi(days), json.Ws(date)}

	dataJs := map[string]json.T{}
	for k, v := range data {
		dataJs[k] = json.Wa(v)
	}

	file.WriteAll(daysDatePath(), json.Wo(dataJs).String())
}

// Reads day ([0]jsonT.Ri()) and date ([1]jsonT.Rs() of every model.
//    lk: Synchronization lock.
func ReadDaysDate(lk sync.T) map[string][]json.T {
	data := map[string][]json.T{}

	dataJs := json.FromString(file.ReadAll(daysDatePath())).Ro()
	for k, v := range dataJs {
		data[k] = v.Ra()
	}

	return data
}

// Reads day ([0]jsonT.Ri()) and date ([1]jsonT.Rs() of a model.
//    lk: Synchronization lock.
//    modelId: Model identifier.
func ReadDaysDateModel(lk sync.T, modelId string) (days int, date string) {
	if jss, ok := ReadDaysDate(lk)[modelId]; ok {
		days = jss[0].Ri()
		date = jss[1].Rs()
		return
	}

	date = "20000101"
	return
}

// Open a temporay file to write new results.
func OpenTmp(lk sync.T) *os.File {
	return file.OpenWrite(tmpFilePath())
}

// Open a model results file to read.
func OpenResults(lk sync.T, modelId string) *os.File {
	return file.OpenRead(filePath(modelId))
}

// Do a function with each element of model results file.
//    lk: Synchronization lock.
//    modelId: Model identifier.
//    fn: Function to execute with each record.
//        fn arguments are:
//          param: Parameter to calculate values.
//          eval : Average of evaluation values.
//          sales: Average of sales number.
//          lastEval: Las evaluation value.
//          lastSale: Las sales number.
//          RETURN: true if 'fn' must stop after execute it.
func EachResult(
	lk sync.T, modelId string, fn func(float64, float64, int, float64, int) bool,
) {
	resultFile := OpenResults(lk, modelId)
	defer resultFile.Close()

	bs := make([]byte, 28)
	fail := ""

	for {
		n, err := resultFile.Read(bs)
		if err != nil {
			if err != io.EOF {
				fail = "'" + err.Error() + "' reading " + modelId
			}
			break
		}
		if n < 0 {
			fail = "Bad bytes number read from " + modelId
			break
		}

		param, value, sales, lastValue, lastSales := result.FromBits(bs)

		if fn(param, value, sales, lastValue, lastSales) {
			break
		}
	}

	if fail != "" {
		log.Error(lk, fail)
		return
	}
}

// Read param-evaluation of each entry.
func ReadResults(lk sync.T, modelId string) []*paramEval.T {
	var r []*paramEval.T

  fn := func(par, eval float64, sales int, _ float64, _ int) bool {
		r = append(r, paramEval.New(par, eval, sales))
		return false
	}
	EachResult(lk, modelId, fn)
	return r
}

// Copy temporary file to model results file and delete the temporary one.
func UpdateResults(lk sync.T, modelId string) {
	if err := os.Rename(tmpFilePath(), filePath(modelId)); err != nil {
		log.Error(
			lk,
			"[resulsDb.UpdateResults] Fail in "+modelId+": "+err.Error(),
		)
	}
}

// Removes entries not contained in 'modelIds'.
//    lk      : Synchronization lock.
//    modelIds: List of models with only one parameter.
func Clean(lk sync.T, modelIds []string) {
	dataJs := json.FromString(file.ReadAll(daysDatePath())).Ro()
	var toDel []string
	for k := range dataJs {
		missing := true
		for _, k2 := range modelIds {
			if k == k2 {
				missing = false
				break
			}
		}
		if missing {
			toDel = append(toDel, k)
		}
	}

	for _, k := range toDel {
		delete(dataJs, k)
	}

	file.WriteAll(daysDatePath(), json.Wo(dataJs).String())

	for _, info := range file.List(path.Dir(daysDatePath())) {
		name := info.Name()
		if name == path.Base(daysDatePath()) {
			continue
		}

		name = name[:len(name)-3]
		missing := true
		for _, name2 := range modelIds {
			if name == name2 {
				missing = false
				break
			}
		}
		if missing {
			file.Remove(filePath(name))
		}
	}
}
