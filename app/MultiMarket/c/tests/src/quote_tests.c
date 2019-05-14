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
  assert(str_eq(quote_test(q), quote_OK));

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

  q = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test(q), quote_OMAX));
  q = opt_get(quote_from_str(
    "20190204:1.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test(q), quote_OMIN));
  q = opt_get(quote_from_str(
    "20190204:2.0500:3.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test(q), quote_CMAX));
  q = opt_get(quote_from_str(
    "20190204:2.0500:1.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test(q), quote_CMIN));

  q = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  Quote *qmodel = opt_get(quote_from_str(
    "20190203:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  Quote *qprev = opt_get(quote_from_str(
    "20190202:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_NO_DATE));

  q = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qmodel = opt_get(quote_from_str(
    "20190205:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qprev = opt_get(quote_from_str(
    "20190202:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_EXTRA_DATE));

  q = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qmodel = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qprev = opt_get(quote_from_str(
    "20190202:2.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_OPEN20));
  qprev = opt_get(quote_from_str(
    "20190202:4.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_OPEN20));

  q = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qmodel = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qprev = opt_get(quote_from_str(
    "20190202:3.0500:1.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_CLOSE20));
  qprev = opt_get(quote_from_str(
    "20190202:3.0500:3.0700:3.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_CLOSE20));


  q = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qmodel = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qprev = opt_get(quote_from_str(
    "20190202:3.0500:2.0700:2.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_MAX20));
  qprev = opt_get(quote_from_str(
    "20190202:3.0500:2.0700:4.0000:2.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_MAX20));

  q = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qmodel = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:false"
  ));
  qprev = opt_get(quote_from_str(
    "20190202:3.0500:2.0700:3.0000:1.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_MIN20));
  qprev = opt_get(quote_from_str(
    "20190202:3.0500:2.0700:3.0000:3.0000:234111:false"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_MIN20));

  q = opt_get(quote_from_str(
    "20190204:3.0500:2.0700:3.0000:2.0000:234111:true"
  ));
  assert(str_eq(quote_test2(q, qmodel, qprev), quote_OK));

  puts("    Finished");
}

