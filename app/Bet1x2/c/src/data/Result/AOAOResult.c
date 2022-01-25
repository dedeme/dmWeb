// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Result/AOAOResult.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AOAOResult *aOAOResult_new (void) {
  return (AOAOResult *)arr_new();
}

AOAOResult *aOAOResult_bf_new (int buffer) {
  return (AOAOResult *)arr_bf_new(buffer);
}

AOAOResult *aOAOResult_new_from (OAOResult *e, ...) {
  va_list args;
  void *tmp;

  AOAOResult *this = aOAOResult_new();
  aOAOResult_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, OAOResult *);
  while (tmp) {
    aOAOResult_push(this, tmp);
    tmp = va_arg(args, OAOResult *);
  }
  va_end(args);

  return this;
}

AOAOResult *aOAOResult_new_c (int size, OAOResult **es) {
  return (AOAOResult *)arr_new_c(size, (void **)es);
}

AOAOResult *aOAOResult_copy (AOAOResult *this) {
  return (AOAOResult *)arr_copy((Arr *)this);
}

int aOAOResult_size (AOAOResult *this) {
  return arr_size((Arr *)this);
}

OAOResult *aOAOResult_get (AOAOResult *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aOAOResult_push (AOAOResult *this, OAOResult *e) {
  arr_push((Arr *)this, e);
}

OAOResult *aOAOResult_pop (AOAOResult *this) {
  return arr_pop((Arr *)this);
}

OAOResult *aOAOResult_peek (AOAOResult *this) {
  return arr_peek((Arr *)this);
}

void aOAOResult_set (AOAOResult *this, int ix, OAOResult *e) {
  arr_set((Arr *)this, ix, e);
}

void aOAOResult_insert (AOAOResult *this, int ix, OAOResult *e) {
  arr_insert((Arr *)this, ix, e);
}

void aOAOResult_remove (AOAOResult *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aOAOResult_cat (AOAOResult *this, AOAOResult *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aOAOResult_insert_arr (AOAOResult *this, int ix, AOAOResult *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aOAOResult_remove_range (AOAOResult *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aOAOResult_clear (AOAOResult *this) {
  arr_clear((Arr *)this);
}

void aOAOResult_reverse (AOAOResult *this) {
  arr_reverse((Arr *)this);
}

void aOAOResult_sort (AOAOResult *this, int (*greater)(OAOResult *e1, OAOResult *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aOAOResult_shuffle (AOAOResult *this) {
  arr_shuffle((Arr *)this);
}

int aOAOResult_all (AOAOResult *this, int (*pred)(OAOResult *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aOAOResult_any (AOAOResult *this, int (*pred)(OAOResult *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aOAOResult_index (AOAOResult *this, int (*pred)(OAOResult *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aOAOResult_last_index (AOAOResult *this, int (*pred)(OAOResult *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OOAOResult *aOAOResult_find(AOAOResult *this, int (*pred)(OAOResult *e)) {
  return (OOAOResult *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OOAOResult *aOAOResult_find_last(AOAOResult *this, int (*pred)(OAOResult *e)) {
  return (OOAOResult *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aOAOResult_filter_in (AOAOResult *this, int (*pred)(OAOResult *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AOAOResult *aOAOResult_take (AOAOResult *this, int n) {
  return (AOAOResult *)arr_take((Arr *)this, n);
}

AOAOResult *aOAOResult_takef (AOAOResult *this, int (*pred)(OAOResult *e)) {
  return (AOAOResult *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AOAOResult *aOAOResult_drop (AOAOResult *this, int n) {
  return (AOAOResult *)arr_drop((Arr *)this, n);
}

AOAOResult *aOAOResult_dropf (AOAOResult *this, int (*pred)(OAOResult *e)) {
  return (AOAOResult *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AOAOResult *aOAOResult_filter_to (AOAOResult *this, int (*pred)(OAOResult *e)) {
  return (AOAOResult *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aOAOResult_map (AOAOResult *this, void *(*converter)(OAOResult *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aOAOResult_map2 (
  AOAOResult *this, void *(*conv1)(OAOResult *e), void *(*conv2)(OAOResult *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aOAOResult_zip (
  AOAOResult *a1, AOAOResult *a2, void *(*converter)(OAOResult *e1, OAOResult *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aOAOResult_zip3 (
  AOAOResult *a1, AOAOResult *a2, AOAOResult *a3,
  void*(*converter)(OAOResult*e1, OAOResult*e2, OAOResult*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AOAOResult *aOAOResult_duplicates (
  AOAOResult *this, int (feq)(OAOResult *e1, OAOResult *e2)
) {
  return (AOAOResult *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aOAOResult_to_js (AOAOResult *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))oAOResult_to_js);
}

AOAOResult *aOAOResult_from_js (char *js) {
  return (AOAOResult *)arr_from_js(js, (void *(*)(char *))oAOResult_from_js);
}


//--// Not remove

