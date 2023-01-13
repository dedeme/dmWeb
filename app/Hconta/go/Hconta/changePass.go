// Copyright 21-06-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Change pass javascript page
package main

import (
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func changePassProcess(ck string, mrq map[string]string) string {
	user := js.Rs(mrq["user"])
	old := js.Rs(mrq["old"])
	new := js.Rs(mrq["new"])

	return cgi.ChangePass(ck, user, old, new)
}
