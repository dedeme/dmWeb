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
	return make([]byte, 28)
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

func FromBits(bs []byte) (param, value float64, sales int, lastValue float64, lastSales int) {
	param = restoreFloat(bs[0:8])
	value = restoreFloat(bs[8:16])
	sales = restoreInt(bs[16:18])
	lastValue = restoreFloat(bs[18:26])
	lastSales = restoreInt(bs[26:28])
	return
}

func ToBits(param, value float64, sales int, lastValue float64, lastSales int) []byte {
	r := MkBs()
	copy(r[0:8], serializeFloat(param))
	copy(r[8:16], serializeFloat(value))
	copy(r[16:18], serializeInt(sales))
	copy(r[18:26], serializeFloat(lastValue))
	copy(r[26:28], serializeInt(lastSales))
	return r
}
