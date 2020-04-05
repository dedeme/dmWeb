// Copyright 07-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "quote_tests.h"
#include "assert.h"
#include "data/Quote.h"

void quote_tests (void) {
  puts("Quote tests:");

  char *qs = "20190204:2.0500:2.0700:3.0000:2.0000:234111:false";
  Quote *q = opt_get(quote_from_str(qs));
  assert(str_eq(qs, quote_to_str(q)));

  qs = "2019024:2.0500:2.0700:3.0000:2.0000:234111:false";
  assert(opt_is_empty(quote_from_str(qs)));
  qs = "2d019024:2.0500:2.0700:3.0000:2.0000:234111:false";
  assert(opt_is_empty(quote_from_str(qs)));
  qs = "20190204:2.0500  :2.0700:3.0000:2.0000:234111:false";
  assert(opt_is_empty(quote_from_str(qs)));
  qs = "20190204:2.0500:2.0700d:3.0000:2.0000:234111:false";
  assert(opt_is_empty(quote_from_str(qs)));
  qs = "20190204:2.0500:2.0700:3.0d000:d2.0000:234111:false";
  assert(opt_is_empty(quote_from_str(qs)));
  qs = "20190204:2.0500:2.0700:3.0000:2.0000:234d111:false";
  assert(opt_is_empty(quote_from_str(qs)));
  qs = "20190204:2.0500:2.0700:3.0000:2.0000:234111:f";
  assert(opt_is_empty(quote_from_str(qs)));
  qs = "20190204:-1:-1:-1:-1:-1:true";
  assert(opt_is_full(quote_from_str(qs)));

  puts("    Finished");
}

