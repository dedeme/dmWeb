// Copyright 24-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io_tests.h"
#include <assert.h>
#include "io.h"

void io_tests (void) {
  puts("io tests:");

  puts("  io_lang");
  {
    GC_NEW

    assert(str_eq("es", io_lang(gc)));
    io_set_lang("en");
    assert(str_eq("en", io_lang(gc)));
    io_set_lang("es");
    assert(str_eq("es", io_lang(gc)));

    GC_FREE
  }

  puts("  io_nicks/io_count_daily_quotes/io_last_historic_date/io_quotes");
  {
    GC_NEW

    // Arr[char]
    Arr *nicks = io_nicks(gc);
    assert(arr_size(nicks) > 0);
    assert(atoi(io_count_daily_quotes(gc)) > 0);
    assert(strcmp(io_last_historic_date(gc), "20190101") > 0);
    RANGE0(i, LONG_G + 1)
      assert(darr_size(io_quotes(gc, i, arr_get(nicks, 0))) > 0);
    _RANGE

    GC_FREE
  }

  puts("  io_mk_league");
  {
    GC_NEW

    // Arr[char]
    Arr *nicks = io_nicks(gc);
    RANGE0(i, LONG_G + 1)
      io_mk_league(gc, nicks, i);
    _RANGE

    GC_FREE
  }

  puts("  io_init");
  {
    GC_NEW

    io_init(gc);

    GC_FREE
  }

  puts("    Finished");
}

