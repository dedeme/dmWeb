// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package test

import (
  "fmt"
//  "net/http"
//  "io/ioutil"
)

func err (ex, ac interface{}) error {
  return fmt.Errorf("\nExpected: %v\nActual  : %v", ex, ac)
}

func Run () {
  //resp, err := http.Get("https://www.eldiario.es/")
  //fmt.Println(err)
  //body, err := ioutil.ReadAll(resp.Body)
  //fmt.Println(string(body))
  textTestRun()
}
