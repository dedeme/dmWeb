// Copyright 06-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas hub.
package fleasHub

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/MMarket/server/fleasMod/mainFleas"
)

func Process(ac chan func(), ck string, mrq map[string]json.T) string {
	key := cgi.RqString(mrq, "source")
	switch key {
	case "MainFleas":
		return mainFleas.Process(ac, ck, mrq)
	default:
		panic(fmt.Sprintf("Value of key is '%v', but it must be '%v'",
			key, "MainFleas"))
	}
}
