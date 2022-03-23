// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/simulation/SimProfitsRow/ASimProfitsRow.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

ASimProfitsRow *aSimProfitsRow_new (void) {
  return (ASimProfitsRow *)arr_new();
}

ASimProfitsRow *aSimProfitsRow_bf_new (int buffer) {
  return (ASimProfitsRow *)arr_bf_new(buffer);
}

ASimProfitsRow *aSimProfitsRow_new_from (SimProfitsRow *e, ...) {
  va_list args;
  void *tmp;

  ASimProfitsRow *this = aSimProfitsRow_new();
  aSimProfitsRow_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, SimProfitsRow *);
  while (tmp) {
    aSimProfitsRow_push(this, tmp);
    tmp = va_arg(args, SimProfitsRow *);
  }
  va_end(args);

  return this;
}

ASimProfitsRow *aSimProfitsRow_new_c (int size, SimProfitsRow **es) {
  return (ASimProfitsRow *)arr_new_c(size, (void **)es);
}

ASimProfitsRow *aSimProfitsRow_copy (ASimProfitsRow *this) {
  return (ASimProfitsRow *)arr_copy((Arr *)this);
}

int aSimProfitsRow_size (ASimProfitsRow *this) {
  return arr_size((Arr *)this);
}

SimProfitsRow *aSimProfitsRow_get (ASimProfitsRow *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aSimProfitsRow_push (ASimProfitsRow *this, SimProfitsRow *e) {
  arr_push((Arr *)this, e);
}

SimProfitsRow *aSimProfitsRow_pop (ASimProfitsRow *this) {
  return arr_pop((Arr *)this);
}

SimProfitsRow *aSimProfitsRow_peek (ASimProfitsRow *this) {
  return arr_peek((Arr *)this);
}

void aSimProfitsRow_set (ASimProfitsRow *this, int ix, SimProfitsRow *e) {
  arr_set((Arr *)this, ix, e);
}

void aSimProfitsRow_insert (ASimProfitsRow *this, int ix, SimProfitsRow *e) {
  arr_insert((Arr *)this, ix, e);
}

void aSimProfitsRow_remove (ASimProfitsRow *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aSimProfitsRow_cat (ASimProfitsRow *this, ASimProfitsRow *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aSimProfitsRow_insert_arr (ASimProfitsRow *this, int ix, ASimProfitsRow *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aSimProfitsRow_remove_range (ASimProfitsRow *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aSimProfitsRow_clear (ASimProfitsRow *this) {
  arr_clear((Arr *)this);
}

void aSimProfitsRow_reverse (ASimProfitsRow *this) {
  arr_reverse((Arr *)this);
}

void aSimProfitsRow_sort (ASimProfitsRow *this, int (*greater)(SimProfitsRow *e1, SimProfitsRow *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aSimProfitsRow_shuffle (ASimProfitsRow *this) {
  arr_shuffle((Arr *)this);
}

int aSimProfitsRow_all (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aSimProfitsRow_any (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aSimProfitsRow_index (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aSimProfitsRow_last_index (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OSimProfitsRow *aSimProfitsRow_find(ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  return (OSimProfitsRow *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OSimProfitsRow *aSimProfitsRow_find_last(ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  return (OSimProfitsRow *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aSimProfitsRow_filter_in (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

ASimProfitsRow *aSimProfitsRow_take (ASimProfitsRow *this, int n) {
  return (ASimProfitsRow *)arr_take((Arr *)this, n);
}

ASimProfitsRow *aSimProfitsRow_takef (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  return (ASimProfitsRow *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

ASimProfitsRow *aSimProfitsRow_drop (ASimProfitsRow *this, int n) {
  return (ASimProfitsRow *)arr_drop((Arr *)this, n);
}

ASimProfitsRow *aSimProfitsRow_dropf (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  return (ASimProfitsRow *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

ASimProfitsRow *aSimProfitsRow_filter_to (ASimProfitsRow *this, int (*pred)(SimProfitsRow *e)) {
  return (ASimProfitsRow *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aSimProfitsRow_map (ASimProfitsRow *this, void *(*converter)(SimProfitsRow *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aSimProfitsRow_map2 (
  ASimProfitsRow *this, void *(*conv1)(SimProfitsRow *e), void *(*conv2)(SimProfitsRow *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aSimProfitsRow_zip (
  ASimProfitsRow *a1, ASimProfitsRow *a2, void *(*converter)(SimProfitsRow *e1, SimProfitsRow *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aSimProfitsRow_zip3 (
  ASimProfitsRow *a1, ASimProfitsRow *a2, ASimProfitsRow *a3,
  void*(*converter)(SimProfitsRow*e1, SimProfitsRow*e2, SimProfitsRow*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

ASimProfitsRow *aSimProfitsRow_duplicates (
  ASimProfitsRow *this, int (feq)(SimProfitsRow *e1, SimProfitsRow *e2)
) {
  return (ASimProfitsRow *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aSimProfitsRow_to_js (ASimProfitsRow *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))simProfitsRow_to_js);
}

ASimProfitsRow *aSimProfitsRow_from_js (char *js) {
  return (ASimProfitsRow *)arr_from_js(js, (void *(*)(char *))simProfitsRow_from_js);
}


//--// Not remove

