// Copyright 08-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/DocEntry/ADocEntry.h"
#include <string.h>
#include <stdarg.h>

ADocEntry *aDocEntry_new (void) {
  return (ADocEntry *)arr_new();
}

ADocEntry *aDocEntry_bf_new (int buffer) {
  return (ADocEntry *)arr_bf_new(buffer);
}

ADocEntry *aDocEntry_new_from (DocEntry *e, ...) {
  va_list args;
  void *tmp;

  ADocEntry *this = aDocEntry_new();
  aDocEntry_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, DocEntry *);
  while (tmp) {
    aDocEntry_push(this, tmp);
    tmp = va_arg(args, DocEntry *);
  }
  va_end(args);

  return this;
}

ADocEntry *aDocEntry_new_c (int size, DocEntry **es) {
  return (ADocEntry *)arr_new_c(size, (void **)es);
}

ADocEntry *aDocEntry_copy (ADocEntry *this) {
  return (ADocEntry *)arr_copy((Arr *)this);
}

int aDocEntry_size (ADocEntry *this) {
  return arr_size((Arr *)this);
}

DocEntry *aDocEntry_get (ADocEntry *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aDocEntry_push (ADocEntry *this, DocEntry *e) {
  arr_push((Arr *)this, e);
}

DocEntry *aDocEntry_pop (ADocEntry *this) {
  return arr_pop((Arr *)this);
}

DocEntry *aDocEntry_peek (ADocEntry *this) {
  return arr_peek((Arr *)this);
}

void aDocEntry_set (ADocEntry *this, int ix, DocEntry *e) {
  arr_set((Arr *)this, ix, e);
}

void aDocEntry_insert (ADocEntry *this, int ix, DocEntry *e) {
  arr_insert((Arr *)this, ix, e);
}

void aDocEntry_remove (ADocEntry *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aDocEntry_cat (ADocEntry *this, ADocEntry *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aDocEntry_insert_arr (ADocEntry *this, int ix, ADocEntry *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aDocEntry_remove_range (ADocEntry *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aDocEntry_clear (ADocEntry *this) {
  arr_clear((Arr *)this);
}

void aDocEntry_reverse (ADocEntry *this) {
  arr_reverse((Arr *)this);
}

void aDocEntry_sort (ADocEntry *this, int (*greater)(DocEntry *e1, DocEntry *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aDocEntry_shuffle (ADocEntry *this) {
  arr_shuffle((Arr *)this);
}

int aDocEntry_all (ADocEntry *this, int (*pred)(DocEntry *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aDocEntry_any (ADocEntry *this, int (*pred)(DocEntry *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aDocEntry_index (ADocEntry *this, int (*pred)(DocEntry *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aDocEntry_last_index (ADocEntry *this, int (*pred)(DocEntry *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

ODocEntry *aDocEntry_find(ADocEntry *this, int (*pred)(DocEntry *e)) {
  return (ODocEntry *)arr_find((Arr *)this, (int(*)(void *))pred);
}

ODocEntry *aDocEntry_find_last(ADocEntry *this, int (*pred)(DocEntry *e)) {
  return (ODocEntry *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aDocEntry_filter_in (ADocEntry *this, int (*pred)(DocEntry *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

ADocEntry *aDocEntry_take (ADocEntry *this, int n) {
  return (ADocEntry *)arr_take((Arr *)this, n);
}

ADocEntry *aDocEntry_takef (ADocEntry *this, int (*pred)(DocEntry *e)) {
  return (ADocEntry *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

ADocEntry *aDocEntry_drop (ADocEntry *this, int n) {
  return (ADocEntry *)arr_drop((Arr *)this, n);
}

ADocEntry *aDocEntry_dropf (ADocEntry *this, int (*pred)(DocEntry *e)) {
  return (ADocEntry *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

ADocEntry *aDocEntry_filter_to (ADocEntry *this, int (*pred)(DocEntry *e)) {
  return (ADocEntry *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aDocEntry_map (ADocEntry *this, void *(*converter)(DocEntry *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aDocEntry_map2 (
  ADocEntry *this, void *(*conv1)(DocEntry *e), void *(*conv2)(DocEntry *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aDocEntry_zip (
  ADocEntry *a1, ADocEntry *a2, void *(*converter)(DocEntry *e1, DocEntry *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aDocEntry_zip3 (
  ADocEntry *a1, ADocEntry *a2, ADocEntry *a3,
  void*(*converter)(DocEntry*e1, DocEntry*e2, DocEntry*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

ADocEntry *aDocEntry_duplicates (
  ADocEntry *this, int (feq)(DocEntry *e1, DocEntry *e2)
) {
  return (ADocEntry *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aDocEntry_to_js (ADocEntry *this, char *(*to)(DocEntry *e)) {
  return arr_to_js((Arr *)this, (char *(*)(void *))to);
}

ADocEntry *aDocEntry_from_js (char *js, DocEntry *(*from)(char *ejs)) {
  return (ADocEntry *)arr_from_js(js, (void *(*)(char *))from);
}


//--// Not remove

