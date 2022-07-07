// Copyright 29-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/OrderNL/AOrderNL.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOrderNL *aOrderNL_new (void) {
  return (AOrderNL *)arr_new();
}

AOrderNL *aOrderNL_bf_new (int buffer) {
  return (AOrderNL *)arr_bf_new(buffer);
}

AOrderNL *aOrderNL_new_from (OrderNL *e, ...) {
  va_list args;
  void *tmp;

  AOrderNL *this = aOrderNL_new();
  aOrderNL_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OrderNL *);
  while (tmp) {
    aOrderNL_push(this, tmp);
    tmp = va_arg(args, OrderNL *);
  }
  va_end(args);

  return this;
}

AOrderNL *aOrderNL_new_c (int size, OrderNL **es) {
  return (AOrderNL *)arr_new_c(size, (void **)es);
}

AOrderNL *aOrderNL_copy (AOrderNL *this) {
  return (AOrderNL *)arr_copy((Arr *)this);
}

int aOrderNL_size (AOrderNL *this) {
  return arr_size((Arr *)this);
}

OrderNL *aOrderNL_get (AOrderNL *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOrderNL_push (AOrderNL *this, OrderNL *e) {
  arr_push((Arr *)this, e);
}

OrderNL *aOrderNL_pop (AOrderNL *this) {
  return arr_pop((Arr *)this);
}

OrderNL *aOrderNL_peek (AOrderNL *this) {
  return arr_peek((Arr *)this);
}

void aOrderNL_set (AOrderNL *this, int ix, OrderNL *e) {
  arr_set((Arr *)this, ix, e);
}

void aOrderNL_insert (AOrderNL *this, int ix, OrderNL *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOrderNL_remove (AOrderNL *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOrderNL_cat (AOrderNL *this, AOrderNL *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOrderNL_insert_arr (AOrderNL *this, int ix, AOrderNL *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOrderNL_remove_range (AOrderNL *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOrderNL_clear (AOrderNL *this) {
  arr_clear((Arr *)this);
}

void aOrderNL_reverse (AOrderNL *this) {
  arr_reverse((Arr *)this);
}

void aOrderNL_sort (AOrderNL *this, int (*greater)(OrderNL *e1, OrderNL *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOrderNL_shuffle (AOrderNL *this) {
  arr_shuffle((Arr *)this);
}

int aOrderNL_all (AOrderNL *this, int (*pred)(OrderNL *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOrderNL_any (AOrderNL *this, int (*pred)(OrderNL *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOrderNL_index (AOrderNL *this, int (*pred)(OrderNL *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOrderNL_last_index (AOrderNL *this, int (*pred)(OrderNL *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOrderNL *aOrderNL_find(AOrderNL *this, int (*pred)(OrderNL *e)) {
  return (OOrderNL *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOrderNL *aOrderNL_find_last(AOrderNL *this, int (*pred)(OrderNL *e)) {
  return (OOrderNL *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOrderNL_filter_in (AOrderNL *this, int (*pred)(OrderNL *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOrderNL *aOrderNL_take (AOrderNL *this, int n) {
  return (AOrderNL *)arr_take((Arr *)this, n);
}

AOrderNL *aOrderNL_takef (AOrderNL *this, int (*pred)(OrderNL *e)) {
  return (AOrderNL *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOrderNL *aOrderNL_drop (AOrderNL *this, int n) {
  return (AOrderNL *)arr_drop((Arr *)this, n);
}

AOrderNL *aOrderNL_dropf (AOrderNL *this, int (*pred)(OrderNL *e)) {
  return (AOrderNL *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOrderNL *aOrderNL_filter_to (AOrderNL *this, int (*pred)(OrderNL *e)) {
  return (AOrderNL *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOrderNL_map (AOrderNL *this, void *(*converter)(OrderNL *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOrderNL_map2 (
  AOrderNL *this, void *(*conv1)(OrderNL *e), void *(*conv2)(OrderNL *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOrderNL_zip (
  AOrderNL *a1, AOrderNL *a2, void *(*converter)(OrderNL *e1, OrderNL *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOrderNL_zip3 (
  AOrderNL *a1, AOrderNL *a2, AOrderNL *a3,
  void*(*converter)(OrderNL*e1, OrderNL*e2, OrderNL*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOrderNL *aOrderNL_duplicates (
  AOrderNL *this, int (feq)(OrderNL *e1, OrderNL *e2)
) {
  return (AOrderNL *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOrderNL_to_js (AOrderNL *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))orderNL_to_js);
}

AOrderNL *aOrderNL_from_js (char *js) {
  return (AOrderNL *)arr_from_js(js, (void *(*)(char *))orderNL_from_js);
}


//--// Not remove

