// Copyright 14-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Locks management
package lock

var request chan func()
var response chan bool

// When is set to 'true', the program finishes.
var End bool

// Initialize lock function
func Initialize() {
	request = make(chan func())
	response = make(chan bool)
	go func() {
		for {
			if End {
				break
			}
			fn := <-request
			fn()
			response <- true
		}
	}()
}

// Execute 'fn' in a synchronized way.
func Run(fn func()) {
	request <- fn
	<-response
}
