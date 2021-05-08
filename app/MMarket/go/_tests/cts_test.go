// Copyright 13-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package _tests

import (
	"github.com/dedeme/MMarket/data/cts"
	"testing"
)

func TestAppName(t *testing.T) {
	if r := eqs(cts.AppName, "MMarket"); r != "" {
		t.Fatal(r)
	}
}
