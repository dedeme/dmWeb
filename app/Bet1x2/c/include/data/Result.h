// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Match result.

#ifndef DATA_RESULT_H
  #define DATA_RESULT_H

#include "data/cts.h"

/// Match result.
struct result_Result {
  int home;
  int out;
};

/// Match result.
typedef struct result_Result Result;

///
Result *result_new (int home, int out);

///
int result_eq (Result *this, Result *other);

///
enum cts_BET_TYPE result_value (Result *this);

///
char *result_to_js (Result *this);

///
Result *result_from_js(char *js);

#endif
