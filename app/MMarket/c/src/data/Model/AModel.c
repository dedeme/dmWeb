// Copyright 16-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Model/AModel.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AModel *aModel_new (void) {
  return (AModel *)arr_new();
}

AModel *aModel_bf_new (int buffer) {
  return (AModel *)arr_bf_new(buffer);
}

AModel *aModel_new_from (Model *e, ...) {
  va_list args;
  void *tmp;

  AModel *this = aModel_new();
  aModel_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Model *);
  while (tmp) {
    aModel_push(this, tmp);
    tmp = va_arg(args, Model *);
  }
  va_end(args);

  return this;
}

AModel *aModel_new_c (int size, Model **es) {
  return (AModel *)arr_new_c(size, (void **)es);
}

AModel *aModel_copy (AModel *this) {
  return (AModel *)arr_copy((Arr *)this);
}

int aModel_size (AModel *this) {
  return arr_size((Arr *)this);
}

Model *aModel_get (AModel *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aModel_push (AModel *this, Model *e) {
  arr_push((Arr *)this, e);
}

Model *aModel_pop (AModel *this) {
  return arr_pop((Arr *)this);
}

Model *aModel_peek (AModel *this) {
  return arr_peek((Arr *)this);
}

void aModel_set (AModel *this, int ix, Model *e) {
  arr_set((Arr *)this, ix, e);
}

void aModel_insert (AModel *this, int ix, Model *e) {
  arr_insert((Arr *)this, ix, e);
}

void aModel_remove (AModel *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aModel_cat (AModel *this, AModel *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aModel_insert_arr (AModel *this, int ix, AModel *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aModel_remove_range (AModel *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aModel_clear (AModel *this) {
  arr_clear((Arr *)this);
}

void aModel_reverse (AModel *this) {
  arr_reverse((Arr *)this);
}

void aModel_sort (AModel *this, int (*greater)(Model *e1, Model *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aModel_shuffle (AModel *this) {
  arr_shuffle((Arr *)this);
}

int aModel_all (AModel *this, int (*pred)(Model *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aModel_any (AModel *this, int (*pred)(Model *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aModel_index (AModel *this, int (*pred)(Model *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aModel_last_index (AModel *this, int (*pred)(Model *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OModel *aModel_find(AModel *this, int (*pred)(Model *e)) {
  return (OModel *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OModel *aModel_find_last(AModel *this, int (*pred)(Model *e)) {
  return (OModel *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aModel_filter_in (AModel *this, int (*pred)(Model *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AModel *aModel_take (AModel *this, int n) {
  return (AModel *)arr_take((Arr *)this, n);
}

AModel *aModel_takef (AModel *this, int (*pred)(Model *e)) {
  return (AModel *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AModel *aModel_drop (AModel *this, int n) {
  return (AModel *)arr_drop((Arr *)this, n);
}

AModel *aModel_dropf (AModel *this, int (*pred)(Model *e)) {
  return (AModel *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AModel *aModel_filter_to (AModel *this, int (*pred)(Model *e)) {
  return (AModel *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aModel_map (AModel *this, void *(*converter)(Model *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aModel_map2 (
  AModel *this, void *(*conv1)(Model *e), void *(*conv2)(Model *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aModel_zip (
  AModel *a1, AModel *a2, void *(*converter)(Model *e1, Model *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aModel_zip3 (
  AModel *a1, AModel *a2, AModel *a3,
  void*(*converter)(Model*e1, Model*e2, Model*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AModel *aModel_duplicates (
  AModel *this, int (feq)(Model *e1, Model *e2)
) {
  return (AModel *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aModel_to_js (AModel *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))model_to_js);
}

AModel *aModel_from_js (char *js) {
  return (AModel *)arr_from_js(js, (void *(*)(char *))model_from_js);
}


//--// Not remove

