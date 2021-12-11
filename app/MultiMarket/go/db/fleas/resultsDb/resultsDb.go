// Copyright 04-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Results of fleas models with only one parameter.
package resultsDb

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/flea/jumpRanking"
	"github.com/dedeme/MultiMarket/data/flea/paramEval"
	"github.com/dedeme/MultiMarket/data/flea/result"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/global/fn"
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

func jumpRanksPath() string {
	return path.Join(parentDir, "jumpRanks.tb")
}

func Initialize(lk sync.T, parent string) {
	parentDir = parent
	if !file.Exists(parentDir) {
		file.Mkdirs(parentDir)
		file.WriteAll(daysDatePath(), json.Wo(map[string]json.T{}).String())
		file.WriteAll(jumpRanksPath(), json.Wa([]json.T{}).String())
	}
	for _, md := range fmodels.List() {
		if !file.Exists(filePath(md.Id())) {
			file.OpenWrite(filePath(md.Id())).Close()
			WriteDaysDate(lk, md.Id(), 0, "20010101")
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

func WriteJumpRanks(lk sync.T, ranks []*jumpRanking.T) {
	var jss []json.T
	for _, r := range ranks {
		jss = append(jss, r.ToJs())
	}
	file.WriteAll(jumpRanksPath(), json.Wa(jss).String())
}

func ReadJumpRanks(lk sync.T) []*jumpRanking.T {
	var r []*jumpRanking.T
	for _, js := range json.FromString(file.ReadAll(jumpRanksPath())).Ra() {
		r = append(r, jumpRanking.FromJs(js))
	}
	return r
}

func ReadJumpRanksJsClient(lk sync.T) json.T {
	var rs []json.T
	for _, r := range ReadJumpRanks(lk) {
		rs = append(rs, r.ToJsClient())
	}
	return json.Wa(rs)
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
//          param        : Parameter to calculate values.
//          eval         : Evaluation values.
//          sales        : Sales number.
//          lastEval     : Last evaluation value.
//          lastSales    : Last sales number.
//          RETURN       : true if 'fn' must stop after execute it.
func EachResult(
	lk sync.T, modelId string,
	fn func(float64, float64, float64, float64, float64) bool,
) {
	resultFile := OpenResults(lk, modelId)
	defer resultFile.Close()

	bs := result.MkBs()
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

		param, eval, sales, lastEval, lastSales := result.FromBits(bs)

		if fn(param, eval, sales, lastEval, lastSales) {
			break
		}
	}

	if fail != "" {
		log.Error(lk, fail)
		return
	}
}

// Do a function with each element of several model results files.
//    lk: Synchronization lock.
//    modelIds: Model identifiers.
//    fn: Function to execute with each record.
//        fn arguments are:
//          param        : Parameter to calculate values.
//          eval         : 'modelIds' average of evaluation value.
//          sales        : 'modelIds' average of sales number.
//          RETURN       : true if 'fn' must stop after execute it.
func EachResults(
	lk sync.T, modelIds []string,
	fun func(float64, float64, float64) bool,
) {
	var resultFiles []*os.File
	for i := range modelIds {
		resultFiles = append(resultFiles, OpenResults(lk, modelIds[i]))
	}
	defer func() {
		for i := range resultFiles {
			resultFiles[i].Close()
		}
	}()

	bs := result.MkBs()
	fail := ""

	for {
		evalFinal := 0.0
		salesFinal := 0.0
		paramFinal := -1.0
		end := false
		for i := range resultFiles {
			n, err := resultFiles[i].Read(bs)
			if err != nil {
				if err != io.EOF {
					fail = "'" + err.Error() + "' reading " + modelIds[i]
				}
				end = true
				break
			}
			if n < 0 {
				fail = "Bad bytes number read from " + modelIds[i]
				break
			}

			param, eval, sales, _, _ := result.FromBits(bs)
			if i == 0 {
				paramFinal = param
			} else {
				if !fn.Eq(param, paramFinal, 0.0000001) {
					fail = fmt.Sprintf("Bad parameter value in %v (%v -> %v)",
						modelIds[i], paramFinal, param,
					)
					break
				}
			}
			evalFinal += eval
			salesFinal += sales
		}

		if fail != "" || end {
			break
		}

		n := float64(len(resultFiles))
		if fun(paramFinal, evalFinal/n, salesFinal/n) {
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

	fn := func(par, eval, sales, _, _ float64) bool {
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
		if name == path.Base(daysDatePath()) || name == path.Base(jumpRanksPath()) {
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
