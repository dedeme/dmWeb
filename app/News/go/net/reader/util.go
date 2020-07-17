// Copyright 09-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Reader utilities
package reader

import (
	"io/ioutil"
	"net/http"
)

func ReadPage(url string) string {
	resp, err := http.Get(url)
	if err == nil {
		body, err := ioutil.ReadAll(resp.Body)
		if err == nil {
			return string(body)
		}
		panic(err.Error())
	}
	panic(err.Error())
}
