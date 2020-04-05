// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "nick_tests.h"
#include "assert.h"
#include "data/Nick.h"

void nick_tests (void) {
  puts("Nick tests:");

  Nick *nk0 = nick_new(0, "TEF");
  Nick *nk = nick_from_js(nick_to_js(nk0));
  assert(nick_id(nk) == 0);
  assert(str_eq(nick_name(nk), "TEF"));
  assert(!nick_is_sel(nk));

  nick_set_is_sel(nk, 1);
  assert(nick_is_sel(nk));

  puts("    Finished");
}
