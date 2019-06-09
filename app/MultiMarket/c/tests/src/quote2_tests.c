// Copyright 30-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "quote2_tests.h"
#include "assert.h"
#include "data/Quote.h"

static Quote *mkq(char *q) {
  return opt_get(quote_from_str(q));
}

// qs is Arr[Quote]
static void push (Arr *qs, char *q) {
  arr_push(qs, mkq(q));
}

static int eq (Quote *q1, char *q2) {
//  puts("----");
//  puts(quote_to_str(q1));
//  puts(q2);
  return str_eq(quote_to_str(q1), q2);
}

void quote2_tests (void) {
  puts("Quote (2) tests:");

  Arr *qs1 = arr_new();
  Arr *qs2 = arr_new();

  push(qs1, "20190204:2.0500:2.0700:3.0000:2.0000:234111:false");
  push(qs2, "20190204:2.0512:2.0700:3.0000:2.0000:234111:false");
  push(qs1, "20190203:2.0500:2.0700:3.0000:2.0000:234111:false");
  push(qs2, "20190202:2.0500:2.0722:3.0000:2.0000:234111:false");
  push(qs1, "20190201:2.0500:2.0700:3.0000:2.0000:234111:false");
  push(qs2, "20190201:2.0500:2.0700:3.0000:2.0200:234111:false");

  // Arr[Arr[Quote]]
  Arr *aqs = arr_new();
  arr_push(aqs, qs1);
  arr_push(aqs, qs2);
  Arr *r = quote_unify(aqs, qs2);
  assert(arr_size(r) == 3);
  assert(eq(arr_get(r, 0),
    "20190204:2.0512:2.0700:3.0000:2.0000:234111:false"));
  assert(eq(arr_get(r, 1),
    "20190202:2.0500:2.0722:3.0000:2.0000:234111:false"));
  assert(eq(arr_get(r, 2),
    "20190201:2.0500:2.0700:3.0000:2.0200:234111:false"));

  aqs = arr_new();
  arr_push(aqs, qs2);
  arr_push(aqs, qs1);
  r = quote_unify(aqs, qs2);
  assert(arr_size(r) == 3);
  assert(eq(arr_get(r, 0),
    "20190204:2.0512:2.0700:3.0000:2.0000:234111:false"));
  assert(eq(arr_get(r, 1),
    "20190202:2.0500:2.0722:3.0000:2.0000:234111:false"));
  assert(eq(arr_get(r, 2),
    "20190201:2.0500:2.0700:3.0000:2.0200:234111:false"));

  Arr *qs3 = arr_new();
  push(qs3, "20190204:2.0500:2.0700:3.0000:2.2000:234111:false");
  push(qs3, "20190203:2.0500:2.0700:3.1500:2.0000:234111:false");
  push(qs3, "20190202:2.0500:2.0722:3.1500:2.0000:234111:false");
  push(qs3, "20190201:2.0500:2.0700:3.0000:2.0300:234111:false");

  aqs = arr_new();
  arr_push(aqs, qs3);
  arr_push(aqs, qs2);
  arr_push(aqs, qs1);
  r = quote_unify(aqs, qs2);
  assert(arr_size(r) == 4);
  assert(eq(arr_get(r, 0),
    "20190204:2.0500:2.0700:3.0000:2.0000:234111:false"));
  assert(eq(arr_get(r, 1),
    "20190203:2.0500:2.0700:3.1500:2.0000:234111:false"));
  assert(eq(arr_get(r, 2),
    "20190202:2.0500:2.0722:3.0000:2.0000:234111:false"));
  assert(eq(arr_get(r, 3),
    "20190201:2.0500:2.0700:3.0000:2.0200:234111:false"));

  // Kv[Quote]
  Kv *cq = quote_corr1(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(!*kv_key(cq));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:false"));

  cq = quote_corr1(
    mkq("20190204:3.0500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Open > Max"));
  assert(eq(kv_value(cq), "20190204:3.0500:2.0700:3.0500:2.0000:234111:true"));

  cq = quote_corr1(
    mkq("20190204:1.0500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Open < Min"));
  assert(eq(kv_value(cq), "20190204:1.0500:2.0700:3.0000:1.0500:234111:true"));

  cq = quote_corr1(
    mkq("20190204:2.0500:3.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Close > Max"));
  assert(eq(kv_value(cq), "20190204:2.0500:3.0700:3.0700:2.0000:234111:true"));

  cq = quote_corr1(
    mkq("20190204:2.0500:1.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Close < Min"));
  assert(eq(kv_value(cq), "20190204:2.0500:1.0700:3.0000:1.0700:234111:true"));

  cq = quote_corr1(
    mkq("20190204:2.0500:1.0700:3.0000:2.0000:234111:true")
  );
  assert(str_eq(kv_key(cq), ""));
  assert(eq(kv_value(cq), "20190204:2.0500:1.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr2(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(!*kv_key(cq));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:false"));

  cq = quote_corr2(
    mkq("20190204:3.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:3.0500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Open > Max"));
  assert(eq(kv_value(cq), "20190204:3.0500:2.0700:3.0500:2.0000:234111:true"));

  cq = quote_corr2(
    mkq("20190204:1.9500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:1.9500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Open < Min"));
  assert(eq(kv_value(cq), "20190204:1.9500:2.0700:3.0000:1.9500:234111:true"));

  cq = quote_corr2(
    mkq("20190204:2.0500:3.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:3.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Close > Max"));
  assert(eq(kv_value(cq), "20190204:2.0500:3.0700:3.0700:2.0000:234111:true"));

  cq = quote_corr2(
    mkq("20190204:2.0500:1.9700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Close < Min"));
  assert(eq(kv_value(cq), "20190204:2.0500:1.9700:3.0000:1.9700:234111:true"));

  cq = quote_corr2(
    mkq("20190204:2.0500:1.0700:3.0000:2.0000:234111:true"),
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), ""));
  assert(eq(kv_value(cq), "20190204:2.0500:1.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr2(
    mkq("20190204:3.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:3.0500:2.0700:3.1000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Open > Max"));
  assert(eq(kv_value(cq), "20190204:3.0000:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr2(
    mkq("20190204:1.9500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:1.9500:2.0700:3.0000:2.1000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Open < Min"));
  assert(eq(kv_value(cq), "20190204:2.0000:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr2(
    mkq("20190204:2.0500:3.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:3.0700:3.1000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Close > Max"));
  assert(eq(kv_value(cq), "20190204:2.0500:3.0000:3.0000:2.0000:234111:true"));

  cq = quote_corr2(
    mkq("20190204:2.0500:1.9700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:1.9700:3.0000:2.1000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Close < Min"));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0000:3.0000:2.0000:234111:true"));

  cq = quote_corr3(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:1.0500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Open +20%"));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr3(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:4.0500:2.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Open -20%"));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr3(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:1.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Close +20%"));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr3(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:4.0700:3.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Close -20%"));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr3(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:2.0700:2.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Max +20%"));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr3(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:2.0700:4.0000:2.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Max -20%"));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr3(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:2.0700:3.0000:1.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Min +20%"));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr3(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:false"),
    mkq("20190204:2.0500:2.0700:3.0000:3.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), "Min -20%"));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:true"));

  cq = quote_corr3(
    mkq("20190204:2.0500:2.0700:3.0000:2.0000:234111:true"),
    mkq("20190204:2.0500:2.0700:3.0000:3.0000:234111:false")
  );
  assert(str_eq(kv_key(cq), ""));
  assert(eq(kv_value(cq), "20190204:2.0500:2.0700:3.0000:2.0000:234111:true"));

  qs1 = arr_new();
  push(qs1, "20190214:2.0500:2.0600:3.0000:2.0200:234111:false");
  push(qs1, "20190213:2.0100:2.0600:3.0000:2.0200:234111:false");
  push(qs1, "20190212:2.0500:2.0600:3.0000:2.0200:234111:true");
  push(qs1, "20190211:2.0100:2.0600:3.0000:2.0000:234111:false");
  push(qs1, "20190210:2.0500:3.0600:3.0000:2.0200:234111:false");

  Tp *es_qs = quote_check(qs1);
  // Arr[char]
  Arr *es = tp_e1(es_qs);
  qs2 = tp_e2(es_qs);
  assert(arr_size(es) == 3);
  assert(arr_size(qs2) == 5);
  assert(eq(arr_get(qs2, 0),
    "20190214:2.0500:2.0600:3.0000:2.0200:234111:false"));
  assert(eq(arr_get(qs2, 1),
    "20190213:2.0100:2.0600:3.0000:2.0100:234111:true"));
  assert(eq(arr_get(qs2, 2),
    "20190212:2.0500:2.0600:3.0000:2.0200:234111:true"));
  assert(eq(arr_get(qs2, 3),
    "20190211:2.0100:2.0600:3.0000:2.0000:234111:true"));
  assert(eq(arr_get(qs2, 4),
    "20190210:2.0500:3.0600:3.0600:2.0200:234111:true"));

  assert(str_eq(arr_get(es, 0), "20190213: Open < Min"));
  assert(str_eq(arr_get(es, 1), "20190211: Close -20%"));
  assert(str_eq(arr_get(es, 2), "20190210: Close > Max"));

  qs1 = arr_new();
  push(qs1, "20190214:2.0500:2.0600:3.0000:2.0200:234111:false");
  push(qs1, "20190213:2.0100:2.0600:3.0000:2.0200:234111:false");
  push(qs1, "20190212:2.0500:2.0600:3.0000:2.0200:234111:false");
  push(qs1, "20190211:2.0100:2.0600:3.0000:2.0200:234111:false");
  push(qs1, "20190210:2.0500:2.0600:3.0000:2.0200:234111:false");
  push(qs1, "20190209:2.0100:2.0600:3.0000:2.0200:234111:false");

  qs2 = arr_new();
  push(qs2, "20190212:2.0500:2.0700:3.0000:2.0200:234111:false");
  push(qs2, "20190211:2.0500:2.0700:3.0000:2.0200:234111:false");
  push(qs2, "20190210:2.0000:2.0700:3.0000:2.0200:234111:true");
  push(qs2, "20190209:2.0000:2.0700:3.0000:2.0200:234111:true");
  push(qs2, "20190208:2.0500:2.0700:3.0000:2.0200:234111:false");
  push(qs2, "20190207:2.0000:2.0700:3.0000:2.0200:234111:true");

  es_qs = quote_blend(qs1, qs2);
  es = tp_e1(es_qs);
  qs3 = tp_e2(es_qs);

  assert(eq(arr_get(qs3, 0),
    "20190214:2.0500:2.0600:3.0000:2.0200:234111:false"));
  assert(eq(arr_get(qs3, 1),
    "20190213:2.0100:2.0600:3.0000:2.0100:234111:true"));
  assert(eq(arr_get(qs3, 2),
    "20190212:2.0500:2.0700:3.0000:2.0200:234111:false"));
  assert(eq(arr_get(qs3, 3),
    "20190211:2.0500:2.0700:3.0000:2.0200:234111:false"));
  assert(eq(arr_get(qs3, 4),
    "20190210:2.0000:2.0700:3.0000:2.0200:234111:true"));
  assert(eq(arr_get(qs3, 5),
    "20190209:2.0000:2.0700:3.0000:2.0200:234111:true"));
  assert(eq(arr_get(qs3, 6),
    "20190208:2.0500:2.0700:3.0000:2.0200:234111:false"));
  assert(eq(arr_get(qs3, 7),
    "20190207:2.0000:2.0700:3.0000:2.0200:234111:true"));

  assert(arr_size(es) == 1);
  assert(str_eq(arr_get(es, 0), "20190213: Open < Min"));

  puts("    Finished");
}
