// Copyright 17-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/eval/ModelEval/AModelEval.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AModelEval *aModelEval_new (void) {
  return (AModelEval *)arr_new();
}

AModelEval *aModelEval_bf_new (int buffer) {
  return (AModelEval *)arr_bf_new(buffer);
}

AModelEval *aModelEval_new_from (ModelEval *e, ...) {
  va_list args;
  void *tmp;

  AModelEval *this = aModelEval_new();
  aModelEval_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, ModelEval *);
  while (tmp) {
    aModelEval_push(this, tmp);
    tmp = va_arg(args, ModelEval *);
  }
  va_end(args);

  return this;
}

AModelEval *aModelEval_new_c (int size, ModelEval **es) {
  return (AModelEval *)arr_new_c(size, (void **)es);
}

AModelEval *aModelEval_copy (AModelEval *this) {
  return (AModelEval *)arr_copy((Arr *)this);
}

int aModelEval_size (AModelEval *this) {
  return arr_size((Arr *)this);
}

ModelEval *aModelEval_get (AModelEval *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aModelEval_push (AModelEval *this, ModelEval *e) {
  arr_push((Arr *)this, e);
}

ModelEval *aModelEval_pop (AModelEval *this) {
  return arr_pop((Arr *)this);
}

ModelEval *aModelEval_peek (AModelEval *this) {
  return arr_peek((Arr *)this);
}

void aModelEval_set (AModelEval *this, int ix, ModelEval *e) {
  arr_set((Arr *)this, ix, e);
}

void aModelEval_insert (AModelEval *this, int ix, ModelEval *e) {
  arr_insert((Arr *)this, ix, e);
}

void aModelEval_remove (AModelEval *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aModelEval_cat (AModelEval *this, AModelEval *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aModelEval_insert_arr (AModelEval *this, int ix, AModelEval *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aModelEval_remove_range (AModelEval *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aModelEval_clear (AModelEval *this) {
  arr_clear((Arr *)this);
}

void aModelEval_reverse (AModelEval *this) {
  arr_reverse((Arr *)this);
}

void aModelEval_sort (AModelEval *this, int (*greater)(ModelEval *e1, ModelEval *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aModelEval_shuffle (AModelEval *this) {
  arr_shuffle((Arr *)this);
}

int aModelEval_all (AModelEval *this, int (*pred)(ModelEval *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aModelEval_any (AModelEval *this, int (*pred)(ModelEval *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aModelEval_index (AModelEval *this, int (*pred)(ModelEval *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aModelEval_last_index (AModelEval *this, int (*pred)(ModelEval *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OModelEval *aModelEval_find(AModelEval *this, int (*pred)(ModelEval *e)) {
  return (OModelEval *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OModelEval *aModelEval_find_last(AModelEval *this, int (*pred)(ModelEval *e)) {
  return (OModelEval *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aModelEval_filter_in (AModelEval *this, int (*pred)(ModelEval *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AModelEval *aModelEval_take (AModelEval *this, int n) {
  return (AModelEval *)arr_take((Arr *)this, n);
}

AModelEval *aModelEval_takef (AModelEval *this, int (*pred)(ModelEval *e)) {
  return (AModelEval *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AModelEval *aModelEval_drop (AModelEval *this, int n) {
  return (AModelEval *)arr_drop((Arr *)this, n);
}

AModelEval *aModelEval_dropf (AModelEval *this, int (*pred)(ModelEval *e)) {
  return (AModelEval *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AModelEval *aModelEval_filter_to (AModelEval *this, int (*pred)(ModelEval *e)) {
  return (AModelEval *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aModelEval_map (AModelEval *this, void *(*converter)(ModelEval *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aModelEval_map2 (
  AModelEval *this, void *(*conv1)(ModelEval *e), void *(*conv2)(ModelEval *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aModelEval_zip (
  AModelEval *a1, AModelEval *a2, void *(*converter)(ModelEval *e1, ModelEval *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aModelEval_zip3 (
  AModelEval *a1, AModelEval *a2, AModelEval *a3,
  void*(*converter)(ModelEval*e1, ModelEval*e2, ModelEval*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AModelEval *aModelEval_duplicates (
  AModelEval *this, int (feq)(ModelEval *e1, ModelEval *e2)
) {
  return (AModelEval *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aModelEval_to_js (AModelEval *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))modelEval_to_js);
}

AModelEval *aModelEval_from_js (char *js) {
  return (AModelEval *)arr_from_js(js, (void *(*)(char *))modelEval_from_js);
}


//--// Not remove

