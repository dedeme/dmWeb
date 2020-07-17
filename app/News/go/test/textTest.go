// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package test

import (
  "fmt"
  "html"
  "github.com/dedeme/News/data/text"
)

func textTestRun () {
  fmt.Println("text: ")
  ex := "[ab mí sí Ún c ded]"
  tx := html.UnescapeString("ab    mí s&iacute; ó Ó Ún c &nbsp;   ded")
  ac := fmt.Sprint(text.WordsRead(tx))
  if ex != ac {
    panic(err(ex, ac))
  }
  ex = "[0 es ...]"
  tx = html.UnescapeString("3.4 es de a...")
  ac = fmt.Sprint(text.WordsRead(tx))
  if ex != ac {
    panic(err(ex, ac))
  }

  fmt.Println("  Finished")
}
