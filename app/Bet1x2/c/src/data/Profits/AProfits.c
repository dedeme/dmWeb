// Copyright 29-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Profits/AProfits.h"
#include <string.h>
#include <stdarg.h>
#include "dmc/js.h"

AProfits *aProfits_new (void) {
  return (AProfits *)arr_new();
}

AProfits *aProfits_bf_new (int buffer) {
  return (AProfits *)arr_bf_new(buffer);
}

AProfits *aProfits_new_from (Profits *e, ...) {
  va_list args;
  void *tmp;

  AProfits *this = aProfits_new();
  aProfits_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, Profits *);
  while (tmp) {
    aProfits_push(this, tmp);
    tmp = va_arg(args, Profits *);
  }
  va_end(args);

  return this;
}

AProfits *aProfits_new_c (int size, Profits **es) {
  return (AProfits *)arr_new_c(size, (void **)es);
}

AProfits *aProfits_copy (AProfits *this) {
  return (AProfits *)arr_copy((Arr *)this);
}

int aProfits_size (AProfits *this) {
  return arr_size((Arr *)this);
}

Profits *aProfits_get (AProfits *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aProfits_push (AProfits *this, Profits *e) {
  arr_push((Arr *)this, e);
}

Profits *aProfits_pop (AProfits *this) {
  return arr_pop((Arr *)this);
}

Profits *aProfits_peek (AProfits *this) {
  return arr_peek((Arr *)this);
}

void aProfits_set (AProfits *this, int ix, Profits *e) {
  arr_set((Arr *)this, ix, e);
}

void aProfits_insert (AProfits *this, int ix, Profits *e) {
  arr_insert((Arr *)this, ix, e);
}

void aProfits_remove (AProfits *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aProfits_cat (AProfits *this, AProfits *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aProfits_insert_arr (AProfits *this, int ix, AProfits *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aProfits_remove_range (AProfits *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aProfits_clear (AProfits *this) {
  arr_clear((Arr *)this);
}

void aProfits_reverse (AProfits *this) {
  arr_reverse((Arr *)this);
}

void aProfits_sort (AProfits *this, int (*greater)(Profits *e1, Profits *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aProfits_shuffle (AProfits *this) {
  arr_shuffle((Arr *)this);
}

int aProfits_all (AProfits *this, int (*pred)(Profits *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aProfits_any (AProfits *this, int (*pred)(Profits *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aProfits_index (AProfits *this, int (*pred)(Profits *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aProfits_last_index (AProfits *this, int (*pred)(Profits *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OProfits *aProfits_find(AProfits *this, int (*pred)(Profits *e)) {
  return (OProfits *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OProfits *aProfits_find_last(AProfits *this, int (*pred)(Profits *e)) {
  return (OProfits *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aProfits_filter_in (AProfits *this, int (*pred)(Profits *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

AProfits *aProfits_take (AProfits *this, int n) {
  return (AProfits *)arr_take((Arr *)this, n);
}

AProfits *aProfits_takef (AProfits *this, int (*pred)(Profits *e)) {
  return (AProfits *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

AProfits *aProfits_drop (AProfits *this, int n) {
  return (AProfits *)arr_drop((Arr *)this, n);
}

AProfits *aProfits_dropf (AProfits *this, int (*pred)(Profits *e)) {
  return (AProfits *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

AProfits *aProfits_filter_to (AProfits *this, int (*pred)(Profits *e)) {
  return (AProfits *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aProfits_map (AProfits *this, void *(*converter)(Profits *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aProfits_map2 (
  AProfits *this, void *(*conv1)(Profits *e), void *(*conv2)(Profits *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aProfits_zip (
  AProfits *a1, AProfits *a2, void *(*converter)(Profits *e1, Profits *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aProfits_zip3 (
  AProfits *a1, AProfits *a2, AProfits *a3,
  void*(*converter)(Profits*e1, Profits*e2, Profits*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

AProfits *aProfits_duplicates (
  AProfits *this, int (feq)(Profits *e1, Profits *e2)
) {
  return (AProfits *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aProfits_to_js (AProfits *this) {
  return arr_to_js((Arr *)this, (char *(*)(void *))profits_to_js);
}

AProfits *aProfits_from_js (char *js) {
  return (AProfits *)arr_from_js(js, (void *(*)(char *))profits_from_js);
}


//--// Not remove

Profits *aProfits_sum(AProfits *this, char *description) {
  int hits = 0;
  int fails = 0;
  double amount = 0;

  Profits **p = this->es;
  while (p < this->end) {
    Profits *prf = *p++;
    hits += prf->hits;
    fails += prf->fails;
    amount += prf->amount;
  }

  return profits_new(description, hits, fails, amount);
}
