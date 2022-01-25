// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Exe/AExe.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AExe *aExe_new (void) {
  return (AExe *)arr_new();
}

AExe *aExe_bf_new (int buffer) {
  return (AExe *)arr_bf_new(buffer);
}

AExe *aExe_new_from (Exe *e, ...) {
  va_list args;
  void *tmp;

  AExe *this = aExe_new();
  aExe_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Exe *);
  while (tmp) {
    aExe_push(this, tmp);
    tmp = va_arg(args, Exe *);
  }
  va_end(args);

  return this;
}

AExe *aExe_new_c (int size, Exe **es) {
  return (AExe *)arr_new_c(size, (void **)es);
}

AExe *aExe_copy (AExe *this) {
  return (AExe *)arr_copy((Arr *)this);
}

int aExe_size (AExe *this) {
  return arr_size((Arr *)this);
}

Exe *aExe_get (AExe *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aExe_push (AExe *this, Exe *e) {
  arr_push((Arr *)this, e);
}

Exe *aExe_pop (AExe *this) {
  return arr_pop((Arr *)this);
}

Exe *aExe_peek (AExe *this) {
  return arr_peek((Arr *)this);
}

void aExe_set (AExe *this, int ix, Exe *e) {
  arr_set((Arr *)this, ix, e);
}

void aExe_insert (AExe *this, int ix, Exe *e) {
  arr_insert((Arr *)this, ix, e);
}

void aExe_remove (AExe *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aExe_cat (AExe *this, AExe *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aExe_insert_arr (AExe *this, int ix, AExe *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aExe_remove_range (AExe *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aExe_clear (AExe *this) {
  arr_clear((Arr *)this);
}

void aExe_reverse (AExe *this) {
  arr_reverse((Arr *)this);
}

void aExe_sort (AExe *this, int (*greater)(Exe *e1, Exe *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aExe_shuffle (AExe *this) {
  arr_shuffle((Arr *)this);
}

int aExe_all (AExe *this, int (*pred)(Exe *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aExe_any (AExe *this, int (*pred)(Exe *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aExe_index (AExe *this, int (*pred)(Exe *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aExe_last_index (AExe *this, int (*pred)(Exe *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OExe *aExe_find(AExe *this, int (*pred)(Exe *e)) {
  return (OExe *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OExe *aExe_find_last(AExe *this, int (*pred)(Exe *e)) {
  return (OExe *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aExe_filter_in (AExe *this, int (*pred)(Exe *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AExe *aExe_take (AExe *this, int n) {
  return (AExe *)arr_take((Arr *)this, n);
}

AExe *aExe_takef (AExe *this, int (*pred)(Exe *e)) {
  return (AExe *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AExe *aExe_drop (AExe *this, int n) {
  return (AExe *)arr_drop((Arr *)this, n);
}

AExe *aExe_dropf (AExe *this, int (*pred)(Exe *e)) {
  return (AExe *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AExe *aExe_filter_to (AExe *this, int (*pred)(Exe *e)) {
  return (AExe *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aExe_map (AExe *this, void *(*converter)(Exe *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aExe_map2 (
  AExe *this, void *(*conv1)(Exe *e), void *(*conv2)(Exe *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aExe_zip (
  AExe *a1, AExe *a2, void *(*converter)(Exe *e1, Exe *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aExe_zip3 (
  AExe *a1, AExe *a2, AExe *a3,
  void*(*converter)(Exe*e1, Exe*e2, Exe*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AExe *aExe_duplicates (
  AExe *this, int (feq)(Exe *e1, Exe *e2)
) {
  return (AExe *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aExe_to_js (AExe *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))exe_to_js);
}

AExe *aExe_from_js (char *js) {
  return (AExe *)arr_from_js(js, (void *(*)(char *))exe_from_js);
}


//--// Not remove

