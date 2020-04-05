// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Flea model.

#ifndef DATA_FLEA_FMODEL_H
  #define DATA_FLEA_FMODEL_H

#include "dmc/async.h"
#include "dmc/Darr.h"
#include "dmc/Iarr.h"
#include "data/Quote.h"
#include "data/Qtable.h"
#include "data/flea/Flea.h"
#include "data/flea/results/Rs.h"

/*--*/

/// Register to tests orders.
///   Arguments:
///     date: char*
///     nick: char*
///     is_sell: bool
///     stocks: int
///     price: double
typedef struct Fmodel_FmodelOrder FmodelOrder;

///
FmodelOrder *fmodelOrder_new (
  char *date,
  char *nick,
  int is_sell,
  int stocks,
  double price
);

///
char *fmodelOrder_date (FmodelOrder *this);

///
char *fmodelOrder_nick (FmodelOrder *this);

///
int fmodelOrder_is_sell (FmodelOrder *this);

///
int fmodelOrder_stocks (FmodelOrder *this);

///
double fmodelOrder_price (FmodelOrder *this);

///
Js *fmodelOrder_to_js (FmodelOrder *this);

/*--*/

/// References calculus.
///   n_cos : Companies number.
///   closes: daily closes of one company, from before to after. Its size is
///           HISTORIC_QUOTES.
///   params: Values of model (like parMins or parMaxs).
///   fn    : Function which is called at the end of each day. It receives
///           day closes (n_cos doubles) and day references (n_cos doubles).
typedef void (*Fcalc) (
  int n_cos,
  QtableRow *closes,
  Darr *params,
  void (*fn) (QtableRow closes, QtableRow refs)
);

/// Flea model data
///   Fields:
///     id: char*
///     name: char*
///     Arr *par_names; // Arr[char*]
///     Darr *par_mins;
///     Darr *par_maxs;
///     Arr *par_js_fmt; // Arr[char] - Parameters format to show in javascript.
///                      // For example: "new Dec(#N#, 2).toIso() + '%'"
///                      // Where '#N#' will be replaced by the parameter value.
///     Fcalc fcalc;
typedef struct Fmodel_Fmodel Fmodel;

Fmodel *fmodel_new (
  char *id,
  char *name,
  Arr *par_names, // Arr[char*]
  Darr *par_mins,
  Darr *par_maxs,
  Arr *par_js,
  Fcalc fcalc
);

///
char *fmodel_id (Fmodel *this);

///
char *fmodel_name (Fmodel *this);

///
Arr *fmodel_par_names (Fmodel *this);

///
Darr *fmodel_par_mins (Fmodel *this);

///
Darr *fmodel_par_maxs (Fmodel *this);

// Arr[char]
Arr *fmodel_par_js_fmt (Fmodel *this);

/// Creates a Flea with random parameters.
///   this : The model.
///   date : Date of creation.
///   cycle: Cycle of creation.
///   id   : Identifier number in cycle.
Flea *fmodel_mk_flea (Fmodel *this, char *date, int cycle, int id);

/// Creates a new Flea mutating another.
///   this : The model.
///   flea : The flea to mutate.
///   date : Date of creation.
///   cycle: Cycle of creation.
///   id   : Identifier number in cycle.
Flea *fmodel_mutate_flea (
  Fmodel *this,
  Flea *flea,
  char *date,
  int cycle,
  int id
);

/// References calculus.
/// Calculate day references and send them to 'fn'.
///   this  : The model.
///   n_cos : Companies number.
///   closes: Daily closes of one company, from before to after. Its size is
///           HISTORIC_QUOTES.
///   params: Values of model (like parMins or parMaxs).
///   fn    : Function which is called at the end of each day. It receives
///           day closes (n_cos doubles) and day references (n_cos doubles).
void fmodel_calc (
  Fmodel *this,
  int n_cos,
  QtableRow *closes,
  Darr *params,
  void (*fn) (QtableRow closes, QtableRow refs)
);

/// Returns historic references of a nick. The number of
/// references returned is HISTORIC_QUOTES and are ordered from before to
/// after.
///   this     : The model.
///   closes   : Daily closes of one company. from before to after. Its size is
///              HISTORIC_QUOTES.
///   params   : Values of model (like parMins or parMaxs).
double *fmodel_refs (Fmodel *this, double *closes, Darr *params);

/// Returns the ratio of profits of a company.
///   this     : The model.
///   opens    : Daily opens of one company. from before to after. Its size is
///              HISTORIC_QUOTES.
///   closes   : Daily closes of one company. from before to after. Its size is
///              HISTORIC_QUOTES.
///   params   : Values of model (like parMins or parMaxs).
double fmodel_profits (
  Fmodel *this,
  double *opens,
  double *closes,
  Darr *params
);

/// Returns the ratio average of opens/closes companies.
double fmodel_profits_avg (
  Fmodel *this,
  Qtable *opens,
  Qtable *closes,
  Darr *params
);

/// Returns Arr[FmodelOrder] with the orders of period.
///   this  : The model.
///   dates : Dates of opens and closes, such as 'quotes_dates' return.
///   opens : Table with opens
///   closes: Table with closes
///   params: Values of model (like parMins or parMaxs).
Arr *fmodel_orders (
  Fmodel *this,
  Arr *dates, // Arr[char *]
  Qtable *opens,
  Qtable *closes,
  Darr *params
);

/// Returns the final cash and operations number of a period.
///   this  : The model.
///   opens : Table with opens
///   closes: Table with closes
///   params: Values of model (like parMins or parMaxs).
Rs *fmodel_assets (
  Fmodel *this,
  Qtable *opens,
  Qtable *closes,
  Darr *params
);

/// Intended only to pass data to javascript.
/// Only the folowing fiels are serialized:
///   id, name, par_names, par_mins, par_maxs.
Js *fmodel_to_js (Fmodel *this);

#endif
