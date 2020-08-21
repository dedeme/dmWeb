// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Threads synchronization.
package sync

import (
	gosync "sync"
)

// Lock structure.
type T struct {
	lock bool
}

var lk = T{true}
var mx gosync.Mutex

// Executes a locked function.
func Run(fn func(lock T)) {
	mx.Lock()
	fn(lk)
	mx.Unlock()
}
