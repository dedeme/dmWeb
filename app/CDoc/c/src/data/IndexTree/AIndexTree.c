// Copyright 08-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/IndexTree/AIndexTree.h"
#include <string.h>
#include <stdarg.h>

AIndexTree *aIndexTree_new (void) {
  return (AIndexTree *)arr_new();
}

AIndexTree *aIndexTree_bf_new (int buffer) {
  return (AIndexTree *)arr_bf_new(buffer);
}

AIndexTree *aIndexTree_new_from (IndexTree *e, ...) {
  va_list args;
  void *tmp;

  AIndexTree *this = aIndexTree_new();
  aIndexTree_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, IndexTree *);
  while (tmp) {
    aIndexTree_push(this, tmp);
    tmp = va_arg(args, IndexTree *);
  }
  va_end(args);

  return this;
}

AIndexTree *aIndexTree_new_c (int size, IndexTree **es) {
  return (AIndexTree *)arr_new_c(size, (void **)es);
}

AIndexTree *aIndexTree_copy (AIndexTree *this) {
  return (AIndexTree *)arr_copy((Arr *)this);
}

int aIndexTree_size (AIndexTree *this) {
  return arr_size((Arr *)this);
}

IndexTree *aIndexTree_get (AIndexTree *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aIndexTree_push (AIndexTree *this, IndexTree *e) {
  arr_push((Arr *)this, e);
}

IndexTree *aIndexTree_pop (AIndexTree *this) {
  return arr_pop((Arr *)this);
}

IndexTree *aIndexTree_peek (AIndexTree *this) {
  return arr_peek((Arr *)this);
}

void aIndexTree_set (AIndexTree *this, int ix, IndexTree *e) {
  arr_set((Arr *)this, ix, e);
}

void aIndexTree_insert (AIndexTree *this, int ix, IndexTree *e) {
  arr_insert((Arr *)this, ix, e);
}

void aIndexTree_remove (AIndexTree *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aIndexTree_cat (AIndexTree *this, AIndexTree *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aIndexTree_insert_arr (AIndexTree *this, int ix, AIndexTree *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aIndexTree_remove_range (AIndexTree *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aIndexTree_clear (AIndexTree *this) {
  arr_clear((Arr *)this);
}

void aIndexTree_reverse (AIndexTree *this) {
  arr_reverse((Arr *)this);
}

void aIndexTree_sort (AIndexTree *this, int (*greater)(IndexTree *e1, IndexTree *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aIndexTree_shuffle (AIndexTree *this) {
  arr_shuffle((Arr *)this);
}

int aIndexTree_all (AIndexTree *this, int (*pred)(IndexTree *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aIndexTree_any (AIndexTree *this, int (*pred)(IndexTree *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aIndexTree_index (AIndexTree *this, int (*pred)(IndexTree *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aIndexTree_last_index (AIndexTree *this, int (*pred)(IndexTree *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OIndexTree *aIndexTree_find(AIndexTree *this, int (*pred)(IndexTree *e)) {
  return (OIndexTree *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OIndexTree *aIndexTree_find_last(AIndexTree *this, int (*pred)(IndexTree *e)) {
  return (OIndexTree *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aIndexTree_filter_in (AIndexTree *this, int (*pred)(IndexTree *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AIndexTree *aIndexTree_take (AIndexTree *this, int n) {
  return (AIndexTree *)arr_take((Arr *)this, n);
}

AIndexTree *aIndexTree_takef (AIndexTree *this, int (*pred)(IndexTree *e)) {
  return (AIndexTree *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AIndexTree *aIndexTree_drop (AIndexTree *this, int n) {
  return (AIndexTree *)arr_drop((Arr *)this, n);
}

AIndexTree *aIndexTree_dropf (AIndexTree *this, int (*pred)(IndexTree *e)) {
  return (AIndexTree *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AIndexTree *aIndexTree_filter_to (AIndexTree *this, int (*pred)(IndexTree *e)) {
  return (AIndexTree *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aIndexTree_map (AIndexTree *this, void *(*converter)(IndexTree *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aIndexTree_map2 (
  AIndexTree *this, void *(*conv1)(IndexTree *e), void *(*conv2)(IndexTree *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aIndexTree_zip (
  AIndexTree *a1, AIndexTree *a2, void *(*converter)(IndexTree *e1, IndexTree *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aIndexTree_zip3 (
  AIndexTree *a1, AIndexTree *a2, AIndexTree *a3,
  void*(*converter)(IndexTree*e1, IndexTree*e2, IndexTree*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AIndexTree *aIndexTree_duplicates (
  AIndexTree *this, int (feq)(IndexTree *e1, IndexTree *e2)
) {
  return (AIndexTree *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aIndexTree_to_js (AIndexTree *this, char *(*to)(IndexTree *e)) {
  return arr_to_js((Arr *)this, (char *(*)(void *))to);
}

AIndexTree *aIndexTree_from_js (char *js, IndexTree *(*from)(char *ejs)) {
  return (AIndexTree *)arr_from_js(js, (void *(*)(char *))from);
}


//--// Not remove

