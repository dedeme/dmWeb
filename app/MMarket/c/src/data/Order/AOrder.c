// Copyright 22-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Order/AOrder.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOrder *aOrder_new (void) {
  return (AOrder *)arr_new();
}

AOrder *aOrder_bf_new (int buffer) {
  return (AOrder *)arr_bf_new(buffer);
}

AOrder *aOrder_new_from (Order *e, ...) {
  va_list args;
  void *tmp;

  AOrder *this = aOrder_new();
  aOrder_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Order *);
  while (tmp) {
    aOrder_push(this, tmp);
    tmp = va_arg(args, Order *);
  }
  va_end(args);

  return this;
}

AOrder *aOrder_new_c (int size, Order **es) {
  return (AOrder *)arr_new_c(size, (void **)es);
}

AOrder *aOrder_copy (AOrder *this) {
  return (AOrder *)arr_copy((Arr *)this);
}

int aOrder_size (AOrder *this) {
  return arr_size((Arr *)this);
}

Order *aOrder_get (AOrder *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOrder_push (AOrder *this, Order *e) {
  arr_push((Arr *)this, e);
}

Order *aOrder_pop (AOrder *this) {
  return arr_pop((Arr *)this);
}

Order *aOrder_peek (AOrder *this) {
  return arr_peek((Arr *)this);
}

void aOrder_set (AOrder *this, int ix, Order *e) {
  arr_set((Arr *)this, ix, e);
}

void aOrder_insert (AOrder *this, int ix, Order *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOrder_remove (AOrder *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOrder_cat (AOrder *this, AOrder *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOrder_insert_arr (AOrder *this, int ix, AOrder *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOrder_remove_range (AOrder *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOrder_clear (AOrder *this) {
  arr_clear((Arr *)this);
}

void aOrder_reverse (AOrder *this) {
  arr_reverse((Arr *)this);
}

void aOrder_sort (AOrder *this, int (*greater)(Order *e1, Order *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOrder_shuffle (AOrder *this) {
  arr_shuffle((Arr *)this);
}

int aOrder_all (AOrder *this, int (*pred)(Order *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOrder_any (AOrder *this, int (*pred)(Order *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOrder_index (AOrder *this, int (*pred)(Order *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOrder_last_index (AOrder *this, int (*pred)(Order *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOrder *aOrder_find(AOrder *this, int (*pred)(Order *e)) {
  return (OOrder *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOrder *aOrder_find_last(AOrder *this, int (*pred)(Order *e)) {
  return (OOrder *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOrder_filter_in (AOrder *this, int (*pred)(Order *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOrder *aOrder_take (AOrder *this, int n) {
  return (AOrder *)arr_take((Arr *)this, n);
}

AOrder *aOrder_takef (AOrder *this, int (*pred)(Order *e)) {
  return (AOrder *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOrder *aOrder_drop (AOrder *this, int n) {
  return (AOrder *)arr_drop((Arr *)this, n);
}

AOrder *aOrder_dropf (AOrder *this, int (*pred)(Order *e)) {
  return (AOrder *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOrder *aOrder_filter_to (AOrder *this, int (*pred)(Order *e)) {
  return (AOrder *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOrder_map (AOrder *this, void *(*converter)(Order *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOrder_map2 (
  AOrder *this, void *(*conv1)(Order *e), void *(*conv2)(Order *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOrder_zip (
  AOrder *a1, AOrder *a2, void *(*converter)(Order *e1, Order *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOrder_zip3 (
  AOrder *a1, AOrder *a2, AOrder *a3,
  void*(*converter)(Order*e1, Order*e2, Order*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOrder *aOrder_duplicates (
  AOrder *this, int (feq)(Order *e1, Order *e2)
) {
  return (AOrder *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOrder_to_js (AOrder *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))order_to_js);
}

AOrder *aOrder_from_js (char *js) {
  return (AOrder *)arr_from_js(js, (void *(*)(char *))order_from_js);
}


//--// Not remove

