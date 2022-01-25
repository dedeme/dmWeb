// Copyright 18-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Result data.

#ifndef DATA_EVAL_RESULT_H
  #define DATA_EVAL_RESULT_H


/// Result data.
struct result_Result {
  /// Currency
  double assets;
  /// Ratio
  double profits;
  /// Sales number
  double sales;
};

/// Result data.
typedef struct result_Result Result;

/// Constructor
Result *result_new (double assets, double profits, double sales);

/// Evaluate 'this'
double result_eval (Result *this);

/// Returns a new Result with 'values of r1' + 'values of r2'.
Result *result_sum (Result *r1, Result *r2);

/// Returns a new Result with 'values of this' / 'n'.
Result *result_div (Result *this, double n);

///
char *result_to_js (Result *this);

#endif
