// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Tbet/ATbet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

ATbet *aTbet_new (void) {
  return (ATbet *)arr_new();
}

ATbet *aTbet_bf_new (int buffer) {
  return (ATbet *)arr_bf_new(buffer);
}

ATbet *aTbet_new_from (Tbet *e, ...) {
  va_list args;
  void *tmp;

  ATbet *this = aTbet_new();
  aTbet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Tbet *);
  while (tmp) {
    aTbet_push(this, tmp);
    tmp = va_arg(args, Tbet *);
  }
  va_end(args);

  return this;
}

ATbet *aTbet_new_c (int size, Tbet **es) {
  return (ATbet *)arr_new_c(size, (void **)es);
}

ATbet *aTbet_copy (ATbet *this) {
  return (ATbet *)arr_copy((Arr *)this);
}

int aTbet_size (ATbet *this) {
  return arr_size((Arr *)this);
}

Tbet *aTbet_get (ATbet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aTbet_push (ATbet *this, Tbet *e) {
  arr_push((Arr *)this, e);
}

Tbet *aTbet_pop (ATbet *this) {
  return arr_pop((Arr *)this);
}

Tbet *aTbet_peek (ATbet *this) {
  return arr_peek((Arr *)this);
}

void aTbet_set (ATbet *this, int ix, Tbet *e) {
  arr_set((Arr *)this, ix, e);
}

void aTbet_insert (ATbet *this, int ix, Tbet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aTbet_remove (ATbet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aTbet_cat (ATbet *this, ATbet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aTbet_insert_arr (ATbet *this, int ix, ATbet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aTbet_remove_range (ATbet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aTbet_clear (ATbet *this) {
  arr_clear((Arr *)this);
}

void aTbet_reverse (ATbet *this) {
  arr_reverse((Arr *)this);
}

void aTbet_sort (ATbet *this, int (*greater)(Tbet *e1, Tbet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aTbet_shuffle (ATbet *this) {
  arr_shuffle((Arr *)this);
}

int aTbet_all (ATbet *this, int (*pred)(Tbet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aTbet_any (ATbet *this, int (*pred)(Tbet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aTbet_index (ATbet *this, int (*pred)(Tbet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aTbet_last_index (ATbet *this, int (*pred)(Tbet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OTbet *aTbet_find(ATbet *this, int (*pred)(Tbet *e)) {
  return (OTbet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OTbet *aTbet_find_last(ATbet *this, int (*pred)(Tbet *e)) {
  return (OTbet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aTbet_filter_in (ATbet *this, int (*pred)(Tbet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

ATbet *aTbet_take (ATbet *this, int n) {
  return (ATbet *)arr_take((Arr *)this, n);
}

ATbet *aTbet_takef (ATbet *this, int (*pred)(Tbet *e)) {
  return (ATbet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

ATbet *aTbet_drop (ATbet *this, int n) {
  return (ATbet *)arr_drop((Arr *)this, n);
}

ATbet *aTbet_dropf (ATbet *this, int (*pred)(Tbet *e)) {
  return (ATbet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

ATbet *aTbet_filter_to (ATbet *this, int (*pred)(Tbet *e)) {
  return (ATbet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aTbet_map (ATbet *this, void *(*converter)(Tbet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aTbet_map2 (
  ATbet *this, void *(*conv1)(Tbet *e), void *(*conv2)(Tbet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aTbet_zip (
  ATbet *a1, ATbet *a2, void *(*converter)(Tbet *e1, Tbet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aTbet_zip3 (
  ATbet *a1, ATbet *a2, ATbet *a3,
  void*(*converter)(Tbet*e1, Tbet*e2, Tbet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

ATbet *aTbet_duplicates (
  ATbet *this, int (feq)(Tbet *e1, Tbet *e2)
) {
  return (ATbet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aTbet_to_js (ATbet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))tbet_to_js);
}

ATbet *aTbet_from_js (char *js) {
  return (ATbet *)arr_from_js(js, (void *(*)(char *))tbet_from_js);
}


//--// Not remove

