// Copyright 04-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data for managing resultsDb
package result

import (
	"encoding/binary"
	"math"
)

// Returns a buffer to read one record of results
func MkBs() []byte {
	return make([]byte, 40)
}

func serializeInt(n int) []byte {
	bs := make([]byte, 2)
	binary.BigEndian.PutUint16(bs, uint16(n))
	return bs
}

func restoreInt(bs []byte) int {
	return int(binary.BigEndian.Uint16(bs))
}

func serializeFloat(n float64) []byte {
	bs := make([]byte, 8)
	binary.BigEndian.PutUint64(bs, math.Float64bits(n))
	return bs
}

func restoreFloat(bs []byte) float64 {
	return math.Float64frombits(binary.BigEndian.Uint64(bs[:]))
}

// bs should be created with 'MkBs()'
func FromBits(bs []byte) (param, eval, sales, lastEval, lastSales float64) {
	param = restoreFloat(bs[0:8])
	eval = restoreFloat(bs[8:16])
	sales = restoreFloat(bs[16:24])
	lastEval = restoreFloat(bs[24:32])
	lastSales = restoreFloat(bs[32:40])
	return
}

func ToBits(param, eval, sales, currentEval, currentSales float64) []byte {
	r := MkBs()
	copy(r[0:8], serializeFloat(param))
	copy(r[8:16], serializeFloat(eval))
	copy(r[16:24], serializeFloat(sales))
	copy(r[24:32], serializeFloat(currentEval))
	copy(r[32:40], serializeFloat(currentSales))
	return r
}
