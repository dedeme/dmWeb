// Copyright 24-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "match_tests.h"
#include <assert.h>
#include "Match.h"

void match_tests (void) {
  puts("Match tests:");
  GC_NEW

  // Arr[Match]
  Arr *rounds = match_rounds(gc, 4);

  EACH_IX(rounds, Arr, a, i)
    GCL_NEW

    Buf *bf = buf_new(gcl);
    EACH(a, Match, m)
      buf_add(bf, str_f(gc, "(%d-%d)", match_up(m), match_down(m)));
    _EACH

    switch (i) {
      case 0 : assert(str_eq("(0-3)(1-2)", buf_str(bf)));break;
      case 1 : assert(str_eq("(0-2)(3-1)", buf_str(bf)));break;
      case 2 : assert(str_eq("(0-1)(2-3)", buf_str(bf)));break;
      default: EXC_ILLEGAL_STATE("i > 2");
    }

    GCL_FREE
  _EACH

  GC_FREE
  puts("    Finished");
}
