// Copyright 08-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package testRs

import (
	"github.com/dedeme/ktlib/js"
)

type T struct {
  IsBig bool
	WithBackups  bool
	WithPathTxt  bool
	Path         string
	PathOk       bool
	NotInBase    bool
	IsMissing    bool
	Synchronized bool
}

// Creates a *T
func New() *T {
	return &T{
    IsBig: false,
		WithBackups:  false,
		WithPathTxt:  false,
		Path:         "",
		PathOk:       false,
		NotInBase:    false,
		IsMissing:    false,
		Synchronized: true,
	}
}

func (rs *T) ToJs() string {
	return js.Wa([]string{
		js.Wb(rs.IsBig),
		js.Wb(rs.WithBackups),
		js.Wb(rs.WithPathTxt),
		js.Ws(rs.Path),
		js.Wb(rs.PathOk),
		js.Wb(rs.NotInBase),
		js.Wb(rs.IsMissing),
		js.Wb(rs.Synchronized),
	})
}
