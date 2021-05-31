// Copyright 25-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Picture change time widget.
package main

import (
	"fmt"
	"github.com/dedeme/Wallpapers/db/sels"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func pictTimeProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "changePictTime":
		value := cgi.RqInt(mrq, "value")
		sels.SetPictTime(value)
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
