// Copyright 08-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package testRs

import (
	"github.com/dedeme/golib/json"
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

func (rs *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wb(rs.IsBig),
		json.Wb(rs.WithBackups),
		json.Wb(rs.WithPathTxt),
		json.Ws(rs.Path),
		json.Wb(rs.PathOk),
		json.Wb(rs.NotInBase),
		json.Wb(rs.IsMissing),
		json.Wb(rs.Synchronized),
	})
}
