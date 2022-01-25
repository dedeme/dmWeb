// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/AAOOBet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAOOBet *aAOOBet_new (void) {
  return (AAOOBet *)arr_new();
}

AAOOBet *aAOOBet_bf_new (int buffer) {
  return (AAOOBet *)arr_bf_new(buffer);
}

AAOOBet *aAOOBet_new_from (AOOBet *e, ...) {
  va_list args;
  void *tmp;

  AAOOBet *this = aAOOBet_new();
  aAOOBet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, AOOBet *);
  while (tmp) {
    aAOOBet_push(this, tmp);
    tmp = va_arg(args, AOOBet *);
  }
  va_end(args);

  return this;
}

AAOOBet *aAOOBet_new_c (int size, AOOBet **es) {
  return (AAOOBet *)arr_new_c(size, (void **)es);
}

AAOOBet *aAOOBet_copy (AAOOBet *this) {
  return (AAOOBet *)arr_copy((Arr *)this);
}

int aAOOBet_size (AAOOBet *this) {
  return arr_size((Arr *)this);
}

AOOBet *aAOOBet_get (AAOOBet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAOOBet_push (AAOOBet *this, AOOBet *e) {
  arr_push((Arr *)this, e);
}

AOOBet *aAOOBet_pop (AAOOBet *this) {
  return arr_pop((Arr *)this);
}

AOOBet *aAOOBet_peek (AAOOBet *this) {
  return arr_peek((Arr *)this);
}

void aAOOBet_set (AAOOBet *this, int ix, AOOBet *e) {
  arr_set((Arr *)this, ix, e);
}

void aAOOBet_insert (AAOOBet *this, int ix, AOOBet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAOOBet_remove (AAOOBet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAOOBet_cat (AAOOBet *this, AAOOBet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAOOBet_insert_arr (AAOOBet *this, int ix, AAOOBet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAOOBet_remove_range (AAOOBet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAOOBet_clear (AAOOBet *this) {
  arr_clear((Arr *)this);
}

void aAOOBet_reverse (AAOOBet *this) {
  arr_reverse((Arr *)this);
}

void aAOOBet_sort (AAOOBet *this, int (*greater)(AOOBet *e1, AOOBet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAOOBet_shuffle (AAOOBet *this) {
  arr_shuffle((Arr *)this);
}

int aAOOBet_all (AAOOBet *this, int (*pred)(AOOBet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAOOBet_any (AAOOBet *this, int (*pred)(AOOBet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAOOBet_index (AAOOBet *this, int (*pred)(AOOBet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAOOBet_last_index (AAOOBet *this, int (*pred)(AOOBet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAOOBet *aAOOBet_find(AAOOBet *this, int (*pred)(AOOBet *e)) {
  return (OAOOBet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAOOBet *aAOOBet_find_last(AAOOBet *this, int (*pred)(AOOBet *e)) {
  return (OAOOBet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAOOBet_filter_in (AAOOBet *this, int (*pred)(AOOBet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAOOBet *aAOOBet_take (AAOOBet *this, int n) {
  return (AAOOBet *)arr_take((Arr *)this, n);
}

AAOOBet *aAOOBet_takef (AAOOBet *this, int (*pred)(AOOBet *e)) {
  return (AAOOBet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAOOBet *aAOOBet_drop (AAOOBet *this, int n) {
  return (AAOOBet *)arr_drop((Arr *)this, n);
}

AAOOBet *aAOOBet_dropf (AAOOBet *this, int (*pred)(AOOBet *e)) {
  return (AAOOBet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAOOBet *aAOOBet_filter_to (AAOOBet *this, int (*pred)(AOOBet *e)) {
  return (AAOOBet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAOOBet_map (AAOOBet *this, void *(*converter)(AOOBet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAOOBet_map2 (
  AAOOBet *this, void *(*conv1)(AOOBet *e), void *(*conv2)(AOOBet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAOOBet_zip (
  AAOOBet *a1, AAOOBet *a2, void *(*converter)(AOOBet *e1, AOOBet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAOOBet_zip3 (
  AAOOBet *a1, AAOOBet *a2, AAOOBet *a3,
  void*(*converter)(AOOBet*e1, AOOBet*e2, AOOBet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAOOBet *aAOOBet_duplicates (
  AAOOBet *this, int (feq)(AOOBet *e1, AOOBet *e2)
) {
  return (AAOOBet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAOOBet_to_js (AAOOBet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))aOOBet_to_js);
}

AAOOBet *aAOOBet_from_js (char *js) {
  return (AAOOBet *)arr_from_js(js, (void *(*)(char *))aOOBet_from_js);
}


//--// Not remove

