// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Bet/ABet.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

ABet *aBet_new (void) {
  return (ABet *)arr_new();
}

ABet *aBet_bf_new (int buffer) {
  return (ABet *)arr_bf_new(buffer);
}

ABet *aBet_new_from (Bet *e, ...) {
  va_list args;
  void *tmp;

  ABet *this = aBet_new();
  aBet_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Bet *);
  while (tmp) {
    aBet_push(this, tmp);
    tmp = va_arg(args, Bet *);
  }
  va_end(args);

  return this;
}

ABet *aBet_new_c (int size, Bet **es) {
  return (ABet *)arr_new_c(size, (void **)es);
}

ABet *aBet_copy (ABet *this) {
  return (ABet *)arr_copy((Arr *)this);
}

int aBet_size (ABet *this) {
  return arr_size((Arr *)this);
}

Bet *aBet_get (ABet *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aBet_push (ABet *this, Bet *e) {
  arr_push((Arr *)this, e);
}

Bet *aBet_pop (ABet *this) {
  return arr_pop((Arr *)this);
}

Bet *aBet_peek (ABet *this) {
  return arr_peek((Arr *)this);
}

void aBet_set (ABet *this, int ix, Bet *e) {
  arr_set((Arr *)this, ix, e);
}

void aBet_insert (ABet *this, int ix, Bet *e) {
  arr_insert((Arr *)this, ix, e);
}

void aBet_remove (ABet *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aBet_cat (ABet *this, ABet *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aBet_insert_arr (ABet *this, int ix, ABet *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aBet_remove_range (ABet *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aBet_clear (ABet *this) {
  arr_clear((Arr *)this);
}

void aBet_reverse (ABet *this) {
  arr_reverse((Arr *)this);
}

void aBet_sort (ABet *this, int (*greater)(Bet *e1, Bet *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aBet_shuffle (ABet *this) {
  arr_shuffle((Arr *)this);
}

int aBet_all (ABet *this, int (*pred)(Bet *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aBet_any (ABet *this, int (*pred)(Bet *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aBet_index (ABet *this, int (*pred)(Bet *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aBet_last_index (ABet *this, int (*pred)(Bet *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OBet *aBet_find(ABet *this, int (*pred)(Bet *e)) {
  return (OBet *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OBet *aBet_find_last(ABet *this, int (*pred)(Bet *e)) {
  return (OBet *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aBet_filter_in (ABet *this, int (*pred)(Bet *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

ABet *aBet_take (ABet *this, int n) {
  return (ABet *)arr_take((Arr *)this, n);
}

ABet *aBet_takef (ABet *this, int (*pred)(Bet *e)) {
  return (ABet *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

ABet *aBet_drop (ABet *this, int n) {
  return (ABet *)arr_drop((Arr *)this, n);
}

ABet *aBet_dropf (ABet *this, int (*pred)(Bet *e)) {
  return (ABet *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

ABet *aBet_filter_to (ABet *this, int (*pred)(Bet *e)) {
  return (ABet *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aBet_map (ABet *this, void *(*converter)(Bet *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aBet_map2 (
  ABet *this, void *(*conv1)(Bet *e), void *(*conv2)(Bet *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aBet_zip (
  ABet *a1, ABet *a2, void *(*converter)(Bet *e1, Bet *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aBet_zip3 (
  ABet *a1, ABet *a2, ABet *a3,
  void*(*converter)(Bet*e1, Bet*e2, Bet*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

ABet *aBet_duplicates (
  ABet *this, int (feq)(Bet *e1, Bet *e2)
) {
  return (ABet *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aBet_to_js (ABet *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))bet_to_js);
}

ABet *aBet_from_js (char *js) {
  return (ABet *)arr_from_js(js, (void *(*)(char *))bet_from_js);
}


//--// Not remove

