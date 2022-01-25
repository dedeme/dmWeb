// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Ann/AAnn.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AAnn *aAnn_new (void) {
  return (AAnn *)arr_new();
}

AAnn *aAnn_bf_new (int buffer) {
  return (AAnn *)arr_bf_new(buffer);
}

AAnn *aAnn_new_from (Ann *e, ...) {
  va_list args;
  void *tmp;

  AAnn *this = aAnn_new();
  aAnn_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Ann *);
  while (tmp) {
    aAnn_push(this, tmp);
    tmp = va_arg(args, Ann *);
  }
  va_end(args);

  return this;
}

AAnn *aAnn_new_c (int size, Ann **es) {
  return (AAnn *)arr_new_c(size, (void **)es);
}

AAnn *aAnn_copy (AAnn *this) {
  return (AAnn *)arr_copy((Arr *)this);
}

int aAnn_size (AAnn *this) {
  return arr_size((Arr *)this);
}

Ann *aAnn_get (AAnn *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aAnn_push (AAnn *this, Ann *e) {
  arr_push((Arr *)this, e);
}

Ann *aAnn_pop (AAnn *this) {
  return arr_pop((Arr *)this);
}

Ann *aAnn_peek (AAnn *this) {
  return arr_peek((Arr *)this);
}

void aAnn_set (AAnn *this, int ix, Ann *e) {
  arr_set((Arr *)this, ix, e);
}

void aAnn_insert (AAnn *this, int ix, Ann *e) {
  arr_insert((Arr *)this, ix, e);
}

void aAnn_remove (AAnn *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aAnn_cat (AAnn *this, AAnn *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aAnn_insert_arr (AAnn *this, int ix, AAnn *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aAnn_remove_range (AAnn *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aAnn_clear (AAnn *this) {
  arr_clear((Arr *)this);
}

void aAnn_reverse (AAnn *this) {
  arr_reverse((Arr *)this);
}

void aAnn_sort (AAnn *this, int (*greater)(Ann *e1, Ann *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aAnn_shuffle (AAnn *this) {
  arr_shuffle((Arr *)this);
}

int aAnn_all (AAnn *this, int (*pred)(Ann *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aAnn_any (AAnn *this, int (*pred)(Ann *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aAnn_index (AAnn *this, int (*pred)(Ann *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aAnn_last_index (AAnn *this, int (*pred)(Ann *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OAnn *aAnn_find(AAnn *this, int (*pred)(Ann *e)) {
  return (OAnn *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OAnn *aAnn_find_last(AAnn *this, int (*pred)(Ann *e)) {
  return (OAnn *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aAnn_filter_in (AAnn *this, int (*pred)(Ann *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AAnn *aAnn_take (AAnn *this, int n) {
  return (AAnn *)arr_take((Arr *)this, n);
}

AAnn *aAnn_takef (AAnn *this, int (*pred)(Ann *e)) {
  return (AAnn *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AAnn *aAnn_drop (AAnn *this, int n) {
  return (AAnn *)arr_drop((Arr *)this, n);
}

AAnn *aAnn_dropf (AAnn *this, int (*pred)(Ann *e)) {
  return (AAnn *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AAnn *aAnn_filter_to (AAnn *this, int (*pred)(Ann *e)) {
  return (AAnn *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aAnn_map (AAnn *this, void *(*converter)(Ann *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aAnn_map2 (
  AAnn *this, void *(*conv1)(Ann *e), void *(*conv2)(Ann *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aAnn_zip (
  AAnn *a1, AAnn *a2, void *(*converter)(Ann *e1, Ann *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aAnn_zip3 (
  AAnn *a1, AAnn *a2, AAnn *a3,
  void*(*converter)(Ann*e1, Ann*e2, Ann*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AAnn *aAnn_duplicates (
  AAnn *this, int (feq)(Ann *e1, Ann *e2)
) {
  return (AAnn *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aAnn_to_js (AAnn *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))ann_to_js);
}

AAnn *aAnn_from_js (char *js) {
  return (AAnn *)arr_from_js(js, (void *(*)(char *))ann_from_js);
}


//--// Not remove

void aAnn_add (AAnn *anns, Ann *ann) {
  int newId = -1;
  Ann **p = anns->es;
  while (p < anns->end) {
    Ann *a = *p++;
    if (a->id > newId) newId = a->id;
  }
  ann_set_id(ann, newId + 1);
  aAnn_push(anns, ann);
}

void aAnn_modify (AAnn *anns, Ann *ann) {
  int size = aAnn_size(anns);
  aAnn_delete(anns, ann->id);
  if (aAnn_size(anns) != size) aAnn_push(anns, ann);
}

void aAnn_delete (AAnn *anns, int id) {
  /**/int filter (Ann *ann) { return ann->id != id; }
  aAnn_filter_in(anns, filter);
}
