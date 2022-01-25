// Copyright 13-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/LogEntry/ALogEntry.h"
#include <string.h>
#include <stdarg.h>

ALogEntry *aLogEntry_new (void) {
  return (ALogEntry *)arr_new();
}

ALogEntry *aLogEntry_bf_new (int buffer) {
  return (ALogEntry *)arr_bf_new(buffer);
}

ALogEntry *aLogEntry_new_from (LogEntry *e, ...) {
  va_list args;
  void *tmp;

  ALogEntry *this = aLogEntry_new();
  aLogEntry_push(this, e);

  va_start(args, e);
  tmp = va_arg(args, LogEntry *);
  while (tmp) {
    aLogEntry_push(this, tmp);
    tmp = va_arg(args, LogEntry *);
  }
  va_end(args);

  return this;
}

ALogEntry *aLogEntry_new_c (int size, LogEntry **es) {
  return (ALogEntry *)arr_new_c(size, (void **)es);
}

ALogEntry *aLogEntry_copy (ALogEntry *this) {
  return (ALogEntry *)arr_copy((Arr *)this);
}

int aLogEntry_size (ALogEntry *this) {
  return arr_size((Arr *)this);
}

LogEntry *aLogEntry_get (ALogEntry *this, int ix) {
  return arr_get((Arr *)this, ix);
}

void aLogEntry_push (ALogEntry *this, LogEntry *e) {
  arr_push((Arr *)this, e);
}

LogEntry *aLogEntry_pop (ALogEntry *this) {
  return arr_pop((Arr *)this);
}

LogEntry *aLogEntry_peek (ALogEntry *this) {
  return arr_peek((Arr *)this);
}

void aLogEntry_set (ALogEntry *this, int ix, LogEntry *e) {
  arr_set((Arr *)this, ix, e);
}

void aLogEntry_insert (ALogEntry *this, int ix, LogEntry *e) {
  arr_insert((Arr *)this, ix, e);
}

void aLogEntry_remove (ALogEntry *this, int ix) {
  arr_remove((Arr *)this, ix);
}

void aLogEntry_cat (ALogEntry *this, ALogEntry *other) {
  arr_cat((Arr *)this, (Arr *)other);
}

void aLogEntry_insert_arr (ALogEntry *this, int ix, ALogEntry *other) {
  arr_insert_arr((Arr *)this, ix, (Arr *)other);
}

void aLogEntry_remove_range (ALogEntry *this, int begin, int end) {
  arr_remove_range((Arr *)this, begin, end);
}

void aLogEntry_clear (ALogEntry *this) {
  arr_clear((Arr *)this);
}

void aLogEntry_reverse (ALogEntry *this) {
  arr_reverse((Arr *)this);
}

void aLogEntry_sort (ALogEntry *this, int (*greater)(LogEntry *e1, LogEntry *e2)) {
  arr_sort((Arr *)this, (int(*)(void *, void *))greater);
}

void aLogEntry_shuffle (ALogEntry *this) {
  arr_shuffle((Arr *)this);
}

int aLogEntry_all (ALogEntry *this, int (*pred)(LogEntry *e)) {
  return arr_all((Arr *)this, (int(*)(void *))pred);
}

int aLogEntry_any (ALogEntry *this, int (*pred)(LogEntry *e)) {
  return arr_any((Arr *)this, (int(*)(void *))pred);
}

int aLogEntry_index (ALogEntry *this, int (*pred)(LogEntry *e)) {
  return arr_index((Arr *)this, (int(*)(void *))pred);
}

int aLogEntry_last_index (ALogEntry *this, int (*pred)(LogEntry *e)) {
  return arr_last_index((Arr *)this, (int(*)(void *))pred);
}

OLogEntry *aLogEntry_find(ALogEntry *this, int (*pred)(LogEntry *e)) {
  return (OLogEntry *)arr_find((Arr *)this, (int(*)(void *))pred);
}

OLogEntry *aLogEntry_find_last(ALogEntry *this, int (*pred)(LogEntry *e)) {
  return (OLogEntry *)arr_find_last((Arr *)this, (int(*)(void *))pred);
}

void aLogEntry_filter_in (ALogEntry *this, int (*pred)(LogEntry *e)) {
  arr_filter_in((Arr *)this, (int(*)(void *))pred);
}

ALogEntry *aLogEntry_take (ALogEntry *this, int n) {
  return (ALogEntry *)arr_take((Arr *)this, n);
}

ALogEntry *aLogEntry_takef (ALogEntry *this, int (*pred)(LogEntry *e)) {
  return (ALogEntry *)arr_takef((Arr *)this, (int(*)(void *))pred);
}

ALogEntry *aLogEntry_drop (ALogEntry *this, int n) {
  return (ALogEntry *)arr_drop((Arr *)this, n);
}

ALogEntry *aLogEntry_dropf (ALogEntry *this, int (*pred)(LogEntry *e)) {
  return (ALogEntry *)arr_dropf((Arr *)this, (int(*)(void *))pred);
}

ALogEntry *aLogEntry_filter_to (ALogEntry *this, int (*pred)(LogEntry *e)) {
  return (ALogEntry *)arr_filter_to((Arr *)this, (int(*)(void *))pred);
}

Arr *aLogEntry_map (ALogEntry *this, void *(*converter)(LogEntry *e)) {
  return arr_map((Arr *)this, (void *(*)(void *))converter);
}

Arr *aLogEntry_map2 (
  ALogEntry *this, void *(*conv1)(LogEntry *e), void *(*conv2)(LogEntry *e)
) {
  return arr_map2((Arr *)this, (void *(*)(void *))conv1, (void *(*)(void *))conv2);
}

Arr *aLogEntry_zip (
  ALogEntry *a1, ALogEntry *a2, void *(*converter)(LogEntry *e1, LogEntry *e2)
) {
  return arr_zip((Arr *)a1, (Arr *)a2, (void *(*)(void *, void *))converter);
}

Arr*aLogEntry_zip3 (
  ALogEntry *a1, ALogEntry *a2, ALogEntry *a3,
  void*(*converter)(LogEntry*e1, LogEntry*e2, LogEntry*e3)
){
  return arr_zip3(
    (Arr *)a1, (Arr *)a2, (Arr *)a3,
    (void *(*)(void *, void *, void *))converter
  );
}

ALogEntry *aLogEntry_duplicates (
  ALogEntry *this, int (feq)(LogEntry *e1, LogEntry *e2)
) {
  return (ALogEntry *)arr_duplicates((Arr *)this, (int(*)(void *, void *))feq);
}

char *aLogEntry_to_js (ALogEntry *this, char *(*to)(LogEntry *e)) {
  return arr_to_js((Arr *)this, (char *(*)(void *))to);
}

ALogEntry *aLogEntry_from_js (char *js, LogEntry *(*from)(char *ejs)) {
  return (ALogEntry *)arr_from_js(js, (void *(*)(char *))from);
}


//--// Not remove

