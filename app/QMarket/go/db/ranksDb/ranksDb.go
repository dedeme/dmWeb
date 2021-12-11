// Copyright 07-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Rankings data base.
package ranksDb

import (
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/rank"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
	"strconv"
)

var fpath string

func fileToQlevel(f string) int {
	r, _ := strconv.Atoi(f[:len(f)-3])
	return r
}

func qlevelToFile(qlevel int) string {
	n := strconv.Itoa(qlevel)
	if len(n) < 2 {
		n = "0" + n
	}
	return n + ".tb"
}

// Auxliar function
func qlevelPath(qlevel int) string {
	return path.Join(fpath, qlevelToFile(qlevel))
}

func mixPath() string {
	return path.Join(fpath, "mix.tb")
}

func avgPath() string {
	return path.Join(fpath, "avg.tb")
}

// Initializes and clean models data base.
//    parent: Parent directory.
func Initialize(parent string) {
	fpath = path.Join(parent, "ranks")
	if !file.Exists(fpath) {
		file.Mkdir(fpath)
		for i := 0; i < cts.Qlevels; i++ {
			Write(i, rank.NewTable())
		}
		WriteMix(rank.NewTable())
		WriteAvg(rank.NewAvgTable())
	}

}

func Write(qlevel int, ranks rank.TableT) {
	file.WriteAll(qlevelPath(qlevel), ranks.ToJs().String())
}

func Read(qlevel int) rank.TableT {
	return rank.TableFromJs(json.FromString(file.ReadAll(qlevelPath(qlevel))))
}

func WriteMix(ranks rank.TableT) {
	file.WriteAll(mixPath(), ranks.ToJs().String())
}

func ReadMix() rank.TableT {
	return rank.TableFromJs(json.FromString(file.ReadAll(mixPath())))
}

func WriteAvg(ranks rank.AvgTableT) {
	file.WriteAll(avgPath(), ranks.ToJs().String())
}

func ReadAvg() rank.AvgTableT {
	return rank.AvgTableFromJs(json.FromString(file.ReadAll(avgPath())))
}
