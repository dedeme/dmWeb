// Copyright 09-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// News data
package newsEntry

import (
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
)

type T struct {
	Date     date.T
	Text     string
	Source   string
	Author   string
	Url      string
	Seval    float64
	Aeval    float64
	Weval    float64
	TtEval   float64
	UserEval int
}

func (e *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(e.Date.String()),
		json.Ws(e.Text),
		json.Ws(e.Source),
		json.Ws(e.Author),
		json.Ws(e.Url),
		json.Wd(e.Seval),
		json.Wd(e.Aeval),
		json.Wd(e.Weval),
		json.Wd(e.TtEval),
		json.Wi(e.UserEval),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		Date:     date.FromString(a[0].Rs()),
		Text:     a[1].Rs(),
		Source:   a[2].Rs(),
		Author:   a[3].Rs(),
		Url:      a[4].Rs(),
		Seval:    a[5].Rd(),
		Aeval:    a[6].Rd(),
		Weval:    a[7].Rd(),
		TtEval:   a[8].Rd(),
		UserEval: a[9].Ri(),
	}
}
