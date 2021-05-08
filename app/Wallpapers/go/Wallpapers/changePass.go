// Copyright 26-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Change pass javascript page
package main

import (
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func changePassProcess(ck string, mrq map[string]json.T) string {
	user := cgi.RqString(mrq, "user")
	old := cgi.RqString(mrq, "old")
	new := cgi.RqString(mrq, "new")

	return cgi.ChangePass(ck, user, old, new)
}
