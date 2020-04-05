// Copyright 24-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Data for summary charts

#ifndef CHART_SUMMARY_H
  #define CHART_SUMMARY_H

#include "dmc/async.h"
#include "data/flea/Fmodel.h"
#include "data/flea/Flea.h"
#include "data/Qtable.h"

/*--*/

/// Record for historic data.
///   Arguments:
///     date: char*
///     assets: double
///     profits: double
///     market: double
typedef struct summary_SummaryData SummaryData;

///
char *summaryData_date (SummaryData *this);

///
double summaryData_assets (SummaryData *this);

///
double summaryData_profits (SummaryData *this);

///
double summaryData_market (SummaryData *this);

///
Js *summaryData_to_js (SummaryData *this);

/*--*/

/// Returns Arr[SumaryData] Historic data.
///   model: The model of flea.
///   dates: Dates of opens and closes.
///   opens: opens table.
///   closes: closes table.
///   params: flea parameters.
Arr *summary_historic(
  Fmodel *model,
  Arr *dates, // Arr[char]
  Qtable *opens,
  Qtable *closes,
  Darr *params
);

/// Returns positions in ranking. Postions number can be 0 (that is 'Iarr' is
/// empty.
///   model  : The model of 'flea'
///   flea   : The flea to get data.
///   ranking: Ranking data.
Iarr *summary_ranking(
  Fmodel *model,
  Flea *flea,
  Arr *ranking // Arr[Arr[Investor]]
);

#endif
// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry of logs.

#ifndef DATA_LOGENTRY_H
  #define DATA_LOGENTRY_H

#include "dmc/async.h"

/*--*/

/// Logs entry
///   Arguments:
///     is_error: int
///     time: char*
///     msg: char*
typedef struct LogEntry_LogEntry LogEntry;

///
LogEntry *logEntry_new (int is_error, char *time, char *msg);

/// 1 if 'this' is a error entry.
int logEntry_is_error (LogEntry *this);

/// Time of message. Its format is 'dd/mm/yyyy(hh:mm:ss)'.
char *logEntry_time (LogEntry *this);

/// Log message.
char *logEntry_msg (LogEntry *this);

///
Js *logEntry_to_js (LogEntry *this);

///
LogEntry *logEntry_from_js (Js *js);

/*--*/

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Error message.

#ifndef DATA_EMSG_H
  #define DATA_EMSG_H

#include "dmc/async.h"

/*--*/

/// Error message
///   Arguments:
///     error: int
///     msg: char*
typedef struct Emsg_Emsg Emsg;

///
Emsg *emsg_new (int error, char *msg);

/// Its value is one of ErrorMsg defined in DEFS.h
int emsg_error (Emsg *this);

///
char *emsg_msg (Emsg *this);

/*--*/

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Table of quotes.
///   - Rows are dates from before to after. Its number is 'HISTORIC_QUOTES'.
///     Each row is saved in a QtableRow.
///   - Columns are companies. Their number is equal to the number of nicks
///     selected. Their order is the order of 'qmatrix_nicks()'.

#ifndef DATA_QTABLE_H
  #define DATA_QTABLE_H

#include "dmc/async.h"

/// Array of doubles.
///   - Their number is equals to the number of nicks selected.
///   - Their order is the order of 'nicks_list()' filtered.
typedef double * QtableRow;

/*--*/

/// Record for manage opens and closes.
///   Arguments:
///     nicks: Arr-Nick
///     values: QtableRow
typedef struct Qtable_Qtable Qtable;

///
Qtable *qtable_new (Arr *nicks, QtableRow *values);

/// Arr[Nick]
Arr *qtable_nicks (Qtable *this);

/// Array of opens/closes ordered by dates(rows) x nicks(columns)
///   - Rows number is HISTORIC_QUOTES. Their are sorted from before to after.
///     Each row is saved in a QmatrixRow (doubles array).
///   - Columns number is 'arr_len(qtable_nicks())'
QtableRow *qtable_values (Qtable *this);

/*--*/

/// Adds 'row' to 'table' and remove the first row of 'table'
void qtable_add(QtableRow *table, QtableRow row);

/// Opt[double] Returns values of a nick, from before to after.
///   - If 'nick' is not found, returns 'opt_empty'
///   - Otherwise return a double* with HISTORIC_QUOTES values.
Opt *qtable_nick_values (Qtable *this, char *nick_name);

/// Returns Arr[double *] with the columns of 'this'
Arr *qtable_cos_values (Qtable *this);

/// Converts a double* to a QtableRow of length 1.
///   nick_quotes: its number must be HISTORIC_QUOTES, and they must be sorted
///                from before to after.
QtableRow *qtable_from_column (double *nick_quotes);

/// Returns the last value > 0.
///   nick_quotes: its number must be HISTORIC_QUOTES, and they must be sorted
///                from before to after.
double qtable_last_ok (double *nick_quotes);

/// Returns the last value > 0 of the column (company number) 'n_co'.
double qtable_last_row_ok (QtableRow *rows, int n_co);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Date and time table of holidays and special days.

#ifndef DATA_MARKETDAY_H
  #define DATA_MARKETDAY_H

#include "dmc/async.h"

/*--*/

/// Market time table for a day.
///   Arguments:
///     date: char*
///     hopen: int
///     mopen: int
///     hclose: int
///     mclose: int
typedef struct MarketDay_MarketDay MarketDay;

///
char *marketDay_date (MarketDay *this);

///
int marketDay_hopen (MarketDay *this);

///
int marketDay_mopen (MarketDay *this);

///
int marketDay_hclose (MarketDay *this);

///
int marketDay_mclose (MarketDay *this);

///
Js *marketDay_to_js (MarketDay *this);

///
MarketDay *marketDay_from_js (Js *js);

/*--*/

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>
/// Broker operations.

#ifndef DATA_BROKER_H
  #define DATA_BROKER_H

#include "dmc/async.h"

/// Returns tatal fees of a buy or sell operation.
double broker_fees (double amount);

/// Returns net cost of operation
double broker_buy (int stocks, double price);

/// Returns net incomes of operation
double broker_sell (int stocks, double price);

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry record for historic data read from server

#ifndef DATA_HISTORIC_H
  #define DATA_HISTORIC_H

#include "dmc/async.h"
#include "Quote.h"

/*--*/

/// Record for historic data read from server
///   Arguments:
///     date: char*
///     open: double
///     close: double
///     max: double
///     min: double
///     vol: int
typedef struct Historic_HistoricEntry HistoricEntry;

///
HistoricEntry *historicEntry_new (
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol
);

///
char *historicEntry_date (HistoricEntry *this);

///
double historicEntry_open (HistoricEntry *this);

///
double historicEntry_close (HistoricEntry *this);

///
double historicEntry_max (HistoricEntry *this);

///
double historicEntry_min (HistoricEntry *this);

///
int historicEntry_vol (HistoricEntry *this);

/*--*/

Quote *historicEntry_to_quote (HistoricEntry *this);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_TIMETABLE_H
  #define DATA_TIMETABLE_H

#include "dmc/async.h"

/*--*/

/// Market day time table.
///   Arguments:
///     hopen: int
///     mopen: int
///     hclose: int
///     mclose: int
typedef struct Timetable_Timetable Timetable;

///
int timetable_hopen (Timetable *this);

///
int timetable_mopen (Timetable *this);

///
int timetable_hclose (Timetable *this);

///
int timetable_mclose (Timetable *this);

///
Js *timetable_to_js (Timetable *this);

///
Timetable *timetable_from_js (Js *js);

/*--*/

/// Creates a time table with values: hopen = 0, mopen = 0, hclose = 23,
/// mclose = 55
Timetable *timetable_new(void);

#endif
// Copyright 24-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily order.

#ifndef DATA_FLEA_DORDER_H
  #define DATA_FLEA_DORDER_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     is_sell: bool
///     co_ix: int
///     pond: double
typedef struct Dorder_Dorder Dorder;

///
int dorder_is_sell (Dorder *this);

///
int dorder_co_ix (Dorder *this);

///
double dorder_pond (Dorder *this);

/*--*/

/// Creates a sell order
///   co_ix: Company index in a QtableRow.
Dorder *dorder_sell (int co_ix);

/// Creates a buy order
///   co_ix: Company index in a QtableRow.
///   pond : Priority of order.
Dorder *dorder_buy (int co_ix, double pond);

// 'dorders' is Arr[Dorder]
void dorder_sort (Arr *dorders);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Models list.

#ifndef DATA_FLEA_FMODELS_H
  #define DATA_FLEA_FMODELS_H

#include "dmc/async.h"

/// Returns Arr[Model]. The model list.
Arr *fmodels_list (void);

/// Returns Opt[Model]
Opt *fmodels_get (char *model_id);

#endif
// Copyright 22-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Record of Model and Flea.

#ifndef DATA_FLEA_INVESTOR_H
  #define DATA_FLEA_INVESTOR_H

#include "dmc/async.h"

#include "data/flea/Flea.h"
#include "data/flea/Fmodel.h"
#include "scheduler/fleas.h"

/*--*/

/// Record of Model and FleasEval.
///   Arguments:
///     model: Fmodel
///     eflea: FleasEval
typedef struct Investor_Investor Investor;

///
Investor *investor_new (Fmodel *model, FleasEval *eflea);

///
Fmodel *investor_model (Investor *this);

///
FleasEval *investor_eflea (Investor *this);

/*--*/

///
Js *investor_to_js (Investor *this);

/// Returns Opt[Investor]
Opt *investor_from_js_opt (Js *js);

#endif
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
///     Iarr *par_js_dec; // Decimals number to show in javascript
///     Fcalc fcalc;
typedef struct Fmodel_Fmodel Fmodel;

Fmodel *fmodel_new (
  char *id,
  char *name,
  Arr *par_names, // Arr[char*]
  Darr *par_mins,
  Darr *par_maxs,
  Iarr *par_js,
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
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Approx investor model.

#ifndef DATA_FLEA_MODELS_APPROX_H
  #define DATA_FLEA_MODELS_APPROX_H

#include "dmc/async.h"
#include "data/flea/Fmodel.h"

/// Approx investor model.
Fmodel *approx_model (void);

#endif
// Copyright 20-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Results of model_assets

#ifndef DATA_FLEA_RESULTS_RS_H
  #define DATA_FLEA_RESULTS_RS_H

#include "dmc/async.h"

/*--*/

/// Results of model_assets
///   Arguments:
///     assets: double
///     buys: int
///     sells: int
typedef struct Rs_Rs Rs;

///
Rs *rs_new (double assets, int buys, int sells);

/// Amount
double rs_assets (Rs *this);

/// Buys number
int rs_buys (Rs *this);

/// Sells number
int rs_sells (Rs *this);

///
Js *rs_to_js (Rs *this);

/*--*/

#endif
// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Flea gen.

#ifndef DATA_FLEA_GEN_H
  #define DATA_FLEA_GEN_H

#include "dmc/async.h"
#include "dmc/Darr.h"

///
typedef Darr Gen;

/// Creates a new gen.
Gen *gen_new(int n, double *params);

/// Number of elements of 'this'
int gen_size(Gen *this);

/// Pointer to parameters of 'this'
double *gen_params(Gen *this);

/// Returns a new gen mutation of 'this'
Gen *gen_mutate(Gen *this);

/// Returns a duplicate of 'this'
Gen *gen_copy(Gen *this);

///
Js *gen_to_js(Gen *this);

///
Gen *gen_from_js(Js *js);

#endif
// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Standar parameters for invesions.

#ifndef DATA_FLEA_FLEA_H
  #define DATA_FLEA_FLEA_H

#include "dmc/async.h"
#include "Gen.h"

/*--*/

/// Standar parameters for investments.
///   Arguments:
///     date: char*
///     cycle: int
///     id: int
///     gen: Gen
typedef struct Flea_Flea Flea;

///
Flea *flea_new (
  char *date,
  int cycle,
  int id,
  Gen *gen
);

/// Date of creation.
char *flea_date (Flea *this);

/// Cycle of creation
int flea_cycle (Flea *this);

/// Identifier number in cycle.
int flea_id (Flea *this);

/// Parameters of investment.
Gen *flea_gen (Flea *this);

///
Js *flea_to_js (Flea *this);

///
Flea *flea_from_js (Js *js);

/*--*/

/// Returns date-cycle-id
char *flea_name (Flea *this);

/// Calculates value of a gen parameter.
///   mx   : Maximun value of parameter
///   mn   : Minimum value of parameter
///   value: Gen parameter - between (0 and 1]
///   - If mx <= mn returns 0.
double flea_param (double mx, double mn, double value);

/// Two fleas are equals if they have equals gen.
int flea_eq (Flea *this, Flea *other);

/// Evaluate a flea.
///   this   : The flea.
///   today  : Current date (from 'date_now').
///   assets : Ratio of assets
///   profits: Ratio of profits.
///   return : A ponderation of 'assets', 'profits' and age.
double flea_evaluate (Flea *this, time_t today, double assets, double profits);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Nick data.

#ifndef DATA_NICK_H
  #define DATA_NICK_H

#include "dmc/async.h"

/*--*/

/// Nick data.
///   Arguments:
///     id: int
///     name: char*
///   Variables:
///     is_sel: bool
typedef struct Nick_Nick Nick;

///
Nick *nick_new (int id, char *name);

///
int nick_id (Nick *this);

///
char *nick_name (Nick *this);

///
void nick_set_name (Nick *this, char *value);

///
int nick_is_sel (Nick *this);

///
void nick_set_is_sel (Nick *this, int value);

///
Js *nick_to_js (Nick *this);

///
Nick *nick_from_js (Js *js);

/// Pair nick-quoteValue
///   Arguments:
///     nick: int
///     value: double
typedef struct Nick_NickQvalue NickQvalue;

///
NickQvalue *nickQvalue_new (int nick, double value);

/// Nick id
int nickQvalue_nick (NickQvalue *this);

/// close, open, maximum or minimum
double nickQvalue_value (NickQvalue *this);

/*--*/

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry record for daily data read from server.

#ifndef DATA_DAILY_H
  #define DATA_DAILY_H

#include "dmc/async.h"

/*--*/

/// Record for daily data read from server.
///   Arguments:
///     code: char*
///     close: double
typedef struct Daily_DailyEntry DailyEntry;

///
DailyEntry *dailyEntry_new (char *code, double close);

///
char *dailyEntry_code (DailyEntry *this);

///
double dailyEntry_close (DailyEntry *this);

/*--*/

#endif
// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Control of application on-off.
/// Intially is set to 'on'. When user send the message 'end' is set to 'off'.

#ifndef DATA_ACTIVITY_H
  #define DATA_ACTIVITY_H

#include "dmc/async.h"

/// Activity estate: 1 -> on, 0 -> off.
int activity_active (void);

/// Finishes application
void activity_off (void);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Quote data and utilities.

#ifndef DATA_QUOTE_H
  #define DATA_QUOTE_H

#include "dmc/async.h"

/*--*/

///Market quote.
///   Arguments:
///     date: char*
///     open: double
///     close: double
///     max: double
///     min: double
///     vol: int
///     error: bool
typedef struct Quote_Quote Quote;

///
Quote *quote_new (
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol,
  int error
);

///
char *quote_date (Quote *this);

///
double quote_open (Quote *this);

///
double quote_close (Quote *this);

///
double quote_max (Quote *this);

///
double quote_min (Quote *this);

///
int quote_vol (Quote *this);

/// It it is 1, quotes are handly modified.
int quote_error (Quote *this);

///
Js *quote_to_js (Quote *this);

///
Quote *quote_from_js (Js *js);

/*--*/

/// Returns Opt[Quote]
Opt *quote_from_str (char *q);

///
char *quote_to_str (Quote *q);

/// Unifies serveral arrays of quotes in one.
///   aqs: Arr[Arr[Quote]]] Arrays to unify
///   qs: Arr[Quote] One of 'aqs' selected for tiebreak
///   return Arr[Quote] A new array with the result.
Arr *quote_unify (Arr *aqs, Arr *best_qs);

/// Checks maximum and minimum and returns a new quote corrected.<p>
/// If quote_error(q) = 1, quote will not be corrected.<p>
/// If quote was corrected, its 'error' field is set to 1
///   q     : Quote to correct
///   return: Kv[Quote]
///           - If there was an error 'kv_key' is a message (e.g. Max > Min)
///             and 'kv_value' is a new quote equals to 'q' corrected.
///           - If there was no error 'kv_key' is a blank string and 'kv_value'
///             is a new quote equals to 'q'.
Kv *quote_corr1 (Quote *q);

/// Checks maximum and minimum and returns a new quote corrected.<p>
/// If quote_error(last) = 1, quote will not be corrected.<p>
/// If quote was corrected, its 'error' field is set to 1
///   last: Quote to correct
///   previous: Quote previous to 'last'
///   return: Kv[Quote]
///           - If there was an error 'kv_key' is a message (e.g. Max > Min)
///             and 'kv_value' is a new quote equals to 'q' corrected.
///           - If there was no error 'kv_key' is a blank string and 'kv_value'
///             is a new quote equals to 'q'.
Kv *quote_corr2 (Quote *last, Quote *previous);

/// Checks increment and returns a new quote corrected.<p>
/// If quote_error(last) = 1, quote will not be corrected.<p>
/// If quote has an incerment +-20%, its 'error' field is set to 1
///   last: Quote to correct
///   previous: Quote previous to 'last'
///   return: Kv[Quote]
///           - If there was an error 'kv_key' is a message (e.g. Max > Min)
///             and 'kv_value' is a new quote equals to 'q' corrected.
///           - If there was no error 'kv_key' is a blank string and 'kv_value'
///             is a new quote equals to 'q'.
Kv *quote_corr3 (Quote *last, Quote *previous);

/// Checks quotes which field 'error' is '= 0', in 'qs'.
///   qs: Arr[Quote] Quotes to check.
///   return: Tp of
///     Arr[char] Errors returned by 'corr1', 'corr2' and 'corr3' with format
///       "date: error". If there is no error, the array is empty.
///     Arr[Quote]. Array corrected.
Tp *quote_check (Arr *qs);

/// Checks dates of 'qs' matching them with the ones of 'model'.
///   model: Arr[Quote] Quotes of nick model.
///   qs: Arr[Quote] Quotes to check.
///   return: Tp of
///     Arr[char] Errors by extra or missing quotes. If there is no error,
///       the array is empty.
///     Arr[Quote]. Array corrected.
Tp *quote_check_dates (Arr *model, Arr *qs);

/// Blends new quotes with others already existent.<p>
/// All Arr are Arr[Quote]
///   model : Arr[Quote]. Model quotes. It can be empty.
///   new   : Arr[Quote]. Last quotes read from the Internet. It can be empty.
///   old   : Arr[Quote]. Existent quotes in file system. It can be empty.
///   return: Tp of
///     Arr[char] Errors returned by 'corr1' and 'corr2' with format
///       "date: error". If there is no error, the array is empty.
///     Arr[Quote]. Array made with the following process:
///        1. Every quote on top with 'open = -1' is removed from 'old' in the
///           dates range of 'new'
///        2. If there are new and old quotes for the same date, that of 'old'
///           is selected.
///        3. The return array is corrected in the range of 'new' dates and
///           adding or removing quotes maching model quotes.
Tp *quote_blend (Arr *model, Arr *new, Arr *old);

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server to read quotes.

#ifndef DATA_SERVER_H
  #define DATA_SERVER_H

#include "dmc/async.h"

/*--*/

/// Nicks-Code pair.
///   Arguments:
///     nick_id: int
///     code: Opt-char*
typedef struct Server_ServerCode ServerCode;

///
ServerCode *serverCode_new (int nick_id, Opt *code);

///
int serverCode_nick_id (ServerCode *this);

/// Opt[char]
Opt *serverCode_code (ServerCode *this);

///
Js *serverCode_to_js (ServerCode *this);

///
ServerCode *serverCode_from_js (Js *js);

/// Server configuration
///   Arguments:
///     cmd: char*
///     url: char*
///     regex: char*
///     sel: int
///     is_date_eu: bool
///     date_separator: char*
///     is_iso_number: bool
///     fields_type: char*
///     table_start: char*
///     table_end: char*
///     row_start: char*
///     row_end: char*
///     cols_start: Arr-char*
///     cols_end: Arr-char*
typedef struct Server_ServerConf ServerConf;

///
char *serverConf_cmd (ServerConf *this);

///
char *serverConf_url (ServerConf *this);

///
char *serverConf_regex (ServerConf *this);

/// enum Server
int serverConf_sel (ServerConf *this);

///
void serverConf_set_sel (ServerConf *this, int value);

///
int serverConf_is_date_eu (ServerConf *this);

///
char *serverConf_date_separator (ServerConf *this);

///
int serverConf_is_iso_number (ServerConf *this);

///
char *serverConf_fields_type (ServerConf *this);

///
char *serverConf_table_start (ServerConf *this);

///
char *serverConf_table_end (ServerConf *this);

///
char *serverConf_row_start (ServerConf *this);

///
char *serverConf_row_end (ServerConf *this);

///
Arr *serverConf_cols_start (ServerConf *this);

///
Arr *serverConf_cols_end (ServerConf *this);

///
Js *serverConf_to_js (ServerConf *this);

///
ServerConf *serverConf_from_js (Js *js);

/// Server data
///   Arguments:
///     id: int
///     short_name: char*
///   Variables:
///     name: char*
///     daily_conf: Opt-ServerConf
///     historic_conf: Opt-ServerConf
///     codes: Arr-ServerCode
typedef struct Server_Server Server;

///
int server_id (Server *this);

///
char *server_short_name (Server *this);

///
void server_set_short_name (Server *this, char *value);

///
char *server_name (Server *this);

///
void server_set_name (Server *this, char *value);

/// Opt[ServerConf]
Opt *server_daily_conf (Server *this);

///
void server_set_daily_conf (Server *this, Opt *value);

/// Opt[ServerConf]
Opt *server_historic_conf (Server *this);

///
void server_set_historic_conf (Server *this, Opt *value);

/// Arr[ServerCode]
Arr *server_codes (Server *this);

///
Js *server_to_js (Server *this);

///
Server *server_from_js (Js *js);

/*--*/

/// 'nicks' is Arr[Nick]
Server *server_new(int id, char *short_name, Arr *nicks);

/// If nick_id has not code, returns "".
char *server_nick_code (Server *this, int nick_id);

/// Code is "" if nick_id has not code.
/// If this has not nick_id registred, this functions does nothing.
void server_set_nick_code (Server *this, int nick_id, char *code);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// HTTP readings.

#ifndef NET_NET_H
  #define NET_NET_H

#include "dmc/async.h"
#include "data/Server.h"
#include "data/Emsg.h"

/// Returns Opt[Arr[NickClose]]<p>
/// Arr[NickClose] can not contain some nick. In such case an error annotation
/// will be done in Log.
Opt *net_server_daily_read (Server *this);


/// Reads a daily conf returning an Opt[Arr[DailyEntry]].<p>
/// Fails are written in Log.
///   conf: Daily configuration
Opt *net_read_daily (ServerConf *conf);

/// Returns Opt[Arr[Quote]]<p>
///   nick_id: Nick identifier
///   return: Opt[Arr[Quote]]. If Arr[quote] has less than 10 elements an
///           annotation is done in Log.
Opt *net_server_historic_read (Server *this, int nick_id);

/// Reads a historic conf returning an Opt[Arr[HistoricEntry]].<p>
/// Fails are written in Log.
///   conf: Historic configuration
///   code: Company code.
Opt *net_read_historic (ServerConf *conf, char *code);

/// Updates quotes of nk_id
///   return: EMsg with values:
///     MSG_OK, "" -> No error
///     MSG_ERROR, "nick" -> Nick not found
///     MSG_ERROR, "server" -> Wrong server data
///     MSG_ERROR, "net" -> Connection fail
///     MSG_ERROR, "server/quotes" -> Wrong server data and quotes corrected
///     MSG_ERROR, "net/quotes" -> Connection fail and quotes corrected
///     MSG_WARNING, "quotes" -> Quotes corrected
Emsg *net_update_historic(AsyncActor *ac, int nk_id);

/// Updates daily quotes. Possible errors are annotated in log.
void net_update_daily (AsyncActor *ac);

#endif
// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server main.

#ifndef SERVER_H
  #define SERVER_H

#include "dmc/async.h"
#include "dmc/Iserver.h"

///
void server_run (AsyncActor *ac, Iserver *server);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page of Home.

#ifndef SERVER_HOME_MAIN_H
  #define SERVER_HOME_MAIN_H

#include "dmc/async.h"

/// Entry point to process requests.
char *mainHome_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page of Main.

#ifndef SERVER_MAIN_MAIN_H
  #define SERVER_MAIN_MAIN_H

#include "dmc/async.h"

/// Entry point to process requests.
char *mainMain_process (Map *mrq);

#endif
// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main entry point of www server.

#ifndef SERVER_HUB_H
  #define SERVER_HUB_H

#include "dmc/async.h"

/// Entry point to process requests.
char *hub_rp (AsyncActor *ac, char *rq);

#endif
// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Selection page of Fleas Ftests.

#ifndef SERVER_FLEAS_FTESTS_SELECTION_H
  #define SERVER_FLEAS_FTESTS_SELECTION_H

#include "dmc/async.h"

/// Entry point to process requests.
char *selection_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Orders page of Fleas Ftests.

#ifndef SERVER_FLEAS_FTESTS_ORDERS_H
  #define SERVER_FLEAS_FTESTS_ORDERS_H

#include "dmc/async.h"

/// Entry point to process requests.
char *orders_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page of Fleas Ftests.

#ifndef SERVER_FLEAS_FTESTS_MAIN_H
  #define SERVER_FLEAS_FTESTS_MAIN_H

#include "dmc/async.h"

/// Entry point to process requests.
char *mainFtests_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// References page of Fleas Ftests.

#ifndef SERVER_FLEAS_FTESTS_REFERENCES_H
  #define SERVER_FLEAS_FTESTS_REFERENCES_H

#include "dmc/async.h"

/// Entry point to process requests.
char *references_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page of Fleas.

#ifndef SERVER_FLEAS_MAIN_H
  #define SERVER_FLEAS_MAIN_H

#include "dmc/async.h"

/// Entry point to process requests.
char *mainFleas_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tests page of Fleas.

#ifndef SERVER_FLEAS_TESTS_H
  #define SERVER_FLEAS_TESTS_H

#include "dmc/async.h"

/// Entry point to process requests.
char *testsFleas_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Companies page of Fleas charts.

#ifndef SERVER_FLEAS_CHARTS_COMPANIES_H
  #define SERVER_FLEAS_CHARTS_COMPANIES_H

#include "dmc/async.h"

/// Entry point to process requests.
char *companies_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page of Fleas charts.

#ifndef SERVER_FLEAS_CHARTS_MAIN_H
  #define SERVER_FLEAS_CHARTS_MAIN_H

#include "dmc/async.h"

/// Entry point to process requests.
char *mainCharts_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 23-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Summary page of Fleas charts.

#ifndef SERVER_FLEAS_CHARTS_SUMMARY_H
  #define SERVER_FLEAS_CHARTS_SUMMARY_H

#include "dmc/async.h"

/// Entry point to process requests.
char *summary_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Calendar page of Settings.

#ifndef SERVER_SETTINGS_CALENDAR_H
  #define SERVER_SETTINGS_CALENDAR_H

#include "dmc/async.h"

/// Entry point to process requests.
char *calendar_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page of Settings.

#ifndef SERVER_SETTINGS_MAIN_H
  #define SERVER_SETTINGS_MAIN_H

#include "dmc/async.h"

/// Entry point to process requests.
char *mainSettings_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Password change page of Settings

#ifndef SERVER_SETTINGS_CHANGEPASS_H
  #define SERVER_SETTINGS_CHANGEPASS_H

#include "dmc/async.h"

/// Entry point to process requests.
char *changepass_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Settings page of Settings

#ifndef SERVER_SETTINGS_SETTINGS_H
  #define SERVER_SETTINGS_SETTINGS_H

#include "dmc/async.h"

/// Entry point to process requests.
char *settings_process (AsyncActor *ac, Map *mrq);

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily quotes database.

#ifndef DB_DAILYQS_H
  #define DB_DAILYQS_H

#include "dmc/async.h"
#include "data/Qtable.h"

///
void dailyqs_init (void);

/// Returns a QtableRow of quotes, which indices match nicks indices. Values
/// missing are set to '-1'.
/// 'nicks' is Arr[char]
QtableRow dailyqs_read (Arr *nicks);

/// if 'nick' is missing, returns -1
double dailyqs_read_nick (char *nick);

/// 'quotes' is Js-> Map[Js->double]
void dailyqs_write (Js *quotes);


#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'calendar.db'

#ifndef DB_CALENDAR_H
  #define DB_CALENDAR_H

#include "dmc/async.h"
#include "data/Timetable.h"
#include "DEFS.h"

///
void calendar_init (void);

/// Returns general time table of market.
Timetable *calendar_general (void);

/// Sets general time table of market.
void calendar_set_general (Timetable *time_table);

/// Returns Arr[char].
Arr *calendar_holidays (void);

/// 'holidays' is Arr[char].
void calendar_set_holidays (Arr *holidays);

/// Returns Arr[MarketDay].
Arr *calendar_special_days (void);

/// 'special_days' is Arr[MarketDay].
void calendar_set_special_days (Arr *special_days);

/// Returns '1' if market is open.
int calendar_is_open (time_t date_time);

#endif
// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Data base initializer.

#ifndef DB_INIT_H
  #define DB_INIT_H

#include "dmc/async.h"

/// Initializes data base.
void initDb_run (void);

#endif
// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// General log.

#ifndef DB_LOG_H
  #define DB_LOG_H

#include "dmc/async.h"

/// Initializes global log.
void log_init (void);

/// Resets log (Delete its contents)
void log_reset (void);

/// Adds an error message.
void log_error (char *msg);

/// Adds a normal message.
void log_info (char *msg);

/// Adds an error message from a exception.
void log_exception (Exc *ex);

/// Reads Log and returns an js-array of'JSONized' LogEntry's
Js *log_read (void);

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas log.

#ifndef DB_FLEAS_FLOG_H
  #define DB_FLEAS_FLOG_H

#include "dmc/async.h"

/// Initializes fleas log.
void flog_init (void);

/// Resets flog (Delete its contents) and generates a new id. Such id is
/// returned.
char *flog_new (void);

/// Stops flog.
/// If identifier is out of date, this function do nothing.
///   id : Identifier returned by flog_new. If its value not mach, this
///        function do nothing.
void flog_stop (char *id);

/// Adds a normal message.
/// If identifier is out of date, this function do nothing.
///   id : Identifier returned by flog_new. If its value not mach, no
///        annotations will be done.
///   msg: Messages to show.
void flog_info (char *id, char *msg);

/// Reads Log and returns an 'JSONized' Opt[Arr[LogEntry's]]
/// If identifier is out of date, this function do nothing.
///   id: Identifier returned by flog_new. If its value not mach, the 'JSONized'
///       Opt[Arr[LogEntry's]] will be empty.
Js *flog_read (char *id);

#endif
// Copyright 20-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas model data base.

#ifndef DB_FLEAS_FMODELS_H
  #define DB_FLEAS_FMODELS_H

#include "dmc/async.h"
#include "data/flea/Fmodel.h"

/// Initializes data base.
void fmodels_init (void);

/// Returns Arr[FleasEval] pool of best fleas. If model is not found, returns an
/// empty array.
Arr *fmodels_read_pool (Fmodel *model);

/// Adds pool of best fleas. If model is not found, function do nothing.
void fmodels_write_pool (
  Fmodel *model,
  Arr *pool // Arr[FleasEval]
);

/// Returns Arr[FleasEval] historic of best fleas. If model is not found,
/// returns an empty array.
Arr *fmodels_read_bests (Fmodel *model);

/// Adds historic of best fleas. If model is not found, function do nothing.
void fmodels_write_bests (
  Fmodel *model,
  Arr *bests // Arr[FleasEval]
);

/// Returns Arr[Arr[FleasEval]] 10 last ranking (Array 10 (days) of 40 (fleas)).
/// If model is not found, returns an empty array.
Arr *fmodels_read_ranking (Fmodel *model);

/// Adds historic of best fleas. If model is not found, function do nothing.
void fmodels_write_ranking (
  Fmodel *model,
  Arr *ranking // Arr[Arr[FleasEval]] (Array 10 (days) of 40 (fleas)).
);

#endif
// Copyright 22-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas general ranking.

#ifndef DB_FLEAS_RANKING_H
  #define DB_FLEAS_RANKING_H

#include "dmc/async.h"

///
void ranking_init ();

/// Arr[Investor]. Returns pool of general ranking.
Arr *ranking_read_pool (void);

/// Writes pool of general ranking. 'investors' is Arr[Investor]
void ranking_write_pool (Arr *investors);

/// Arr[Arr[Investor]]. Returns general ranking.
/// 10 last ranking (Array 10 (days) of 40 (fleas)).
/// Every Arr[Investor] is descendently ordered.
Arr *ranking_read (void);

/// Writes general ranking. 'ranking' is Arr[Arr[Investor]].
/// 10 last ranking (Array 10 (days) of 40 (fleas)).
/// Every Arr[Investor] is descendently ordered.
void ranking_write (Arr *ranking);

#endif
// Copyright 16-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global parameters

#ifndef DB_GLOBAL_H
  #define DB_GLOBAL_H

#include "dmc/async.h"

/// Initialization
void global_init (void);

/// Its value can be 'en' or 'es'
char *global_lang (void);

/// 'lang' must be 'en' or 'es'
void global_set_lang (char *lang);

/// Returns identifier of activity. It can be ACT_SLEEPING1, ACT_SLEEPING2,
/// ACT_ACTIVATING, ACT_ACTIVE or ACT_DEACTIVATING.
char *global_activity (void);

/// 'activity' must be ACT_SLEEPING1, ACT_SLEEPING2, ACT_ACTIVATING,
/// ACT_ACTIVE or ACT_DEACTIVATING.
void global_set_activity (char *activity);

/// Return the current time stamp
char *global_time_stamp (void);

/// Sets time stamp with the current time and returns it.
char *global_set_time_stamp (void);

/// Check if current time stamp is equals to 'ts'.
int global_check_time_stamp (char *ts);

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Nicks data base.

#ifndef DB_NICKS_H
  #define DB_NICKS_H

#include "dmc/async.h"
#include "data/Nick.h"

/// Initializes data base.
void nicks_init (void);

/// Returns id of nick model or -1 if it has not been set.
int nicks_model (void);

/// Sets nick model
void nicks_set_model(int nk_id);

/// Arr[Nick]
Arr *nicks_list (void);

/// Adds a nick if it is not duplicated and returns 1. Otherwise returns 0.
int nicks_add (char *nk_name);

/// Removes nick with id 'id' if it exists
void nicks_del (int nk_id);

/// Returns Opt[Nick] with the nick which id is 'id'. If it does not exist
/// returns 'opt_empty()'
Opt *nicks_get (int nk_id);

/// Modifies nick if 'nick_name(nick)' is not duplicated, returning 1.
/// Otherwise returns 0
int nicks_modify (Nick *nick);

/// Set the nick with 'nick_id' selected on/off. If nick_id does not exist,
/// it does nothing
///   value: is 0 or 1.
void nicks_set_selected (int nk_id, int value);

/// Returns Arr[Nick] with selected list.
Arr *nicks_selected_list (void);

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily server selector.

#ifndef DB_SBOX_H
  #define DB_SBOX_H

#include "dmc/async.h"

///
void sbox_init ();

/// Generates next server.
void sbox_next ();

/// Returns Opt[Server] Current server.
Opt *sbox_get ();

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of companies quotes (Directory quotes)

#ifndef DB_QUOTES_H
  #define DB_QUOTES_H

#include "dmc/async.h"
#include "data/Quote.h"
#include "data/Emsg.h"
#include "data/Qtable.h"

///
void quotes_init (void);

/// Returns quotes of 'nick_name'.
///   nick_name: Name of nick (e.g. TEF)
///   return: Arr[Quote]. If an error results, the array is empty.
///           Order of quotes is from after to before.
Arr *quotes_read (char *nick_name);

/// Writes 'qs' of 'nick_name'
///   nick_name: Name of nick (e.g. TEF)
///   qs: It is Arr[Quote]. Its order is from after to before.
void quotes_write (char *nick_name, Arr *qs);

/// Creates a new empty file if it does not already exist.
///   nick_name: Name of nick (e.g. TEF)
void quotes_add (char *nick_name);

/// Deletes a file if it exist.
///   nick_name: Name of nick (e.g. TEF)
void quotes_del (char *nick_name);

/// Changes file 'old_name' by 'new_name' if 'old_name' exists
void quotes_modify (char *old_name, char *new_name);

/// Returns is Tp[EMsg, char]
///   nick_id Nick identifier.
///   returns: Tp of:
///     EMsg: whith values:
///       MSG_OK, "" -> No error
///       MSG_WARNING, "quotes" -> Warning: quotes with error
///       MSG_WARNING, "number" -> Warning: Bad quotes number
///       MSG_ERROR, "io" -> Error: File not found
///       MSG_ERROR, "syntax" -> Error: Syntax error in lines
///       MSG_ERROR, "empty" -> Error: There is no quote
///     char: The text file or "" if it does not exists.
Tp *quotes_editor_read(int nick_id);

/// Sets quotes of a company (inclusive quotes model)
///   nick_id: Nick identifier.
///   qs_text: Text with quotes
///   return: An EMsg with values:
///       MSG_OK, "" -> No error
///       MSG_WARNING, "quotes" -> Warning: quotes with error
///       MSG_WARNING, "number" -> Warning: Bad quotes number
///       MSG_ERROR, "io" -> Error: File not found
///       MSG_ERROR, "syntax" -> Error: Syntax error in lines
///       MSG_ERROR, "empty" -> Error: There is no quote
///       MSG_ERROR, "model" -> Error: Nick model was not defined
///       <i>NOTE: MSG_OK and MSG_WARNING modifies data, MSG_ERROR not</i>.
Emsg *quotes_editor_set_quotes(int nick_id, char *qs_text);

/// Returns an Arr[char] with dates of flea model, from before to after. If
/// data have errors returns an empty array.
Arr *quotes_dates (void);

/// Returns a Opt[Qtable] with closes of selected companies, from before to
/// after
Opt *quotes_closes (void);

/// Returns a Opt[Qtable] with opens of selected companies, from before to
/// after
Opt *quotes_opens (void);

/// Returns a Opt[char] with the last date of company model. If nick model is
/// not defined, returns the last date of the first company. If there is no
/// company or model has not quotes returns opt_empty.
Opt *quotes_last_date (void);

/// Returns Map[Js->double]. Keys are nicks_names. If there is some fail
/// returns an empty Map.
Map *quotes_last_quotes (void);

/// Returns Map[Js->int]. (nick_name->volume average of VOLUME_QUOTES days).<br>
/// Nicks are retrieved from quotes data base and can be some nick missing
/// relative to nicks data base.
Js *quotes_volume (void);

#endif
// Copyright 17-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Servers data base

#ifndef DB_SERVERS_H
  #define DB_SERVERS_H

#include "dmc/async.h"
#include "data/Server.h"

///
void servers_init (void);

/// Arr[Server]
Arr *servers_list (void);

/// Returns Arr[Server] Historic active servers
Arr *servers_historic_list (void);

/// Returns Arr[Server] Daily active servers
Arr *servers_daily_list (void);

/// Returns the url of a company for acc->companies page (using infobolsa)
char *servers_acc_url (char *nick);

/// If 'short_name' is duplicate, operation is not done and it returns 0.
int servers_add (char *short_name);

/// Removes Server with id 'id'
void servers_remove (int id);

/// Sets name and short_name of Server with id 'id'. If short name is duplicated
/// returns 0. Otherwise returns 1.
int servers_set_names (int id, char *short_name, char *name);

/// Activates / Deactivates server
///   id: Server identifier. It it does not exists, this function does nothing.
///   historic: (1/0) If historic configuration will be updated
///   conf: Initial configuration
void servers_activate(int id, int historic, ServerConf *conf);

/// Sets configurations.
///   id: Server identifier. It it does not exists, this function does nothing.
///   historic: (1/0) If historic configuration will be updated
///   conf: New configuration
void servers_set_conf(int id, int historic, ServerConf *conf);

/// Set codes. If 'id' does not exists, this function does nothing.
///   id: Server id
///   codes: Arr[ServerCodes]. New codes
void servers_set_codes(int id, Arr *codes);

/// Adds a new nick with id 'nk_id' if it does not exist
void servers_add_nick (int nk_id);

/// Removes nick with id 'nk_id' if it exists.
void servers_del_nick (int nk_id);

/// Returns Arr[SeversIdNameCode]. If 'nick_id' does not have code, its code
/// value is an empty string.
Arr *servers_nick_codes (int nick_id);

/// Sets code of nick_id.
void servers_set_nick_code (int server_id, int nick_id, char *code);

/// Returns '1' if daily closes reading is correct. Otherwise returns '0' and
/// makes an annotation in Log.
int servers_test_daily_conf (int id);

/// Returns '1' if historic quotes reading is correct. Otherwise returns '0' and
/// makes an annotation in Log.
int servers_test_historic_conf (int id, int nk_id);

#endif
// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global definitions.

#ifndef DEFS_H
  #define DEFS_H

#include "dmc/async.h"

// Application -----------------------------------------------------------------

///
#define APP_NAME "MMarket"

/// Data version
#define DATA_VERSION "MultiMarket\nData version: 202001\n"

/// Data directory relative to sys_home()
#define DATA_PATH "data"

/// Data directory relative to sys_home()
#define FLEAS_PATH "data/fleas"

/// Data directory relative to sys_home()
#define RANKING_PATH "data/rank"

/// Data directory relative to sys_home()
#define ACC_PATH "data/acc"

/// Log maximum entries
#define LOG_MAX_ENTRIES 300

/// Time to server sleep (milliseconds)
#define ACTOR_SLEEP 10

/// Error messages
enum ErrorMsg { MSG_OK, MSG_WARNING, MSG_ERROR };

// Server ----------------------------------------------------------------------

/// Communications port
#define PORT 50204

/// Time of connection expiration
#define EXPIRATION 900

/// Time to server sleep (milliseconds)
#define SERVER_SLEEP 10

/// Time to server sleep (milliseconds) when reading errors
#define SERVER_ERROR_SLEEP 120000

/// Server configuration states
enum Server { SERVER_STOPPED, SERVER_ACTIVE, SERVER_SELECTED };

// scheduler -------------------------------------------------------------------

/// Time to scheduler sleep (milliseconds)
#define SCHEDULER_SLEEP 50

/// External servers (Infobolsa, Invertia, etc.) delay in SECONDS
#define SERVERS_DELAY 1200

/// Activity state
#define ACT_SLEEPING1 "Sleeping (1)"

/// Activity state
#define ACT_HISTORIC "Historic"

/// Activity state
#define ACT_SLEEPING2 "Sleeping (2)"

/// Activity state
#define ACT_ACTIVATING "Activating"

/// Activity state
#define ACT_ACTIVE "Active"

/// Activity state
#define ACT_DEACTIVATING "Deactivating"

/// Hour to start ACT_HISTORIC fase
#define ACT_HISTORIC_START 3

/// Hour to finish ACT_HISTORIC fase
#define ACT_HISTORIC_END 8

// Fleas -----------------------------------------------------------------------

/// Number of quotes in historic
#define HISTORIC_QUOTES 610

/// Fleas initial capital for each cycle
#define INITIAL_CAPITAL 150000

/// Bet
#define BET 15000

/// Minimun cash to bet
#define MIN_TO_BET 16000

/// Minimum operations to survive (multiplicator: days * minSells)
#define MIN_SELLS 0.05

/// Maximun operations to survive (multiplicator: days * maxSells)
#define MAX_SELLS 0.1

/// Maximum of mutation
#define MUTATION_MULTIPLIER 0.3

/// Number of cycle per parameter after insertion to finish a process
#define CYCLES 5

/// Number of cycle to insert historic results
#define INSERTION_CYCLE 10

/// Number of fleas per model
#define FLEAS_PER_MODEL 2000

// Fleas evaluation ------------------------------------------------------------

/// Historic simulation ratio
#define ASSETS_RATIO 0.375

/// Average of profits ratio
#define PROFITS_RATIO 0.375

/// Flea age ratio
#define AGE_RATIO 0.25

/// Number of fleas in pool
#define POOL_NUMBER 1000

/// Number of fleas in ranking
#define RANKING_NUMBER 40

/// Number of fleas that changes per session
#define RANKING_CHANGES 4

/// Number of fleas in ranking
#define HISTORIC_RANKING_ENTRIES 10

// Not assigned ----------------------------------------------------------------

/// Minimal number of companies in a set
#define SET_COMPANIES 10

/// Number of quotes to calculate volume
#define VOLUME_QUOTES 100

/// Maximun number of fleas in "_best"
#define MAXIMUM_HISTORIC_BESTS 252

/// Number of daily results in data base 'data/fleas/MODEL'
#define FLEA_MODEL_DATES 10

/// Avg days of champions
#define CHAMPIONS_AVG 10

/// Number total of champion fleas per group
#define TOTAL_CHAMPIONS 500

/// Quotes number in account charts
#define ACC_CHART_QUOTES 250

/// Ponderation for raking
#define RANKING_ASSETS_RATIO 0.45

/// Ponderation for raking
#define RANKING_PROFITS_RATIO 0.35

/// Ponderation for raking
#define RANKING_AGE_RATIO 0.2

/// Number of fleas in ranking
#define HISTORIC_RANKING_ENTRIES 10

/// Maximum number of data in ranking charts
#define HISTORIC_RANKING_CHAR_MAX 450

/// Server short name to get url in accounting charts
#define ACC_URL "INFOB"

/// External command to download
#define WGET "Wget"

/// External command to download
#define PUPPETEER "Puppeteer"

#endif
// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Application main.

#ifndef MMARKET_H
  #define MMARKET_H

#include "dmc/async.h"

///
int main (int argc, char *argv[]);

#endif
// Copyright 19-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of fleas.

#ifndef SCHEDULER_FLEAS_H
  #define SCHEDULER_FLEAS_H

#include "dmc/async.h"
#include "data/flea/Fmodel.h"

/*--*/

/// Flea evaluation data.
///   Arguments:
///     flea: Flea
///     buys: int
///     sells: int
///     assets: double
///     profits: double
///     eval: double
typedef struct fleas_FleasEval FleasEval;

/// Evaluated flea.
Flea *fleasEval_flea (FleasEval *this);

/// Number of buys.
int fleasEval_buys (FleasEval *this);

/// Number of sells.
int fleasEval_sells (FleasEval *this);

/// Assets in simulation (money).
double fleasEval_assets (FleasEval *this);

/// Profits average of every company (ratios --can be < 0).
double fleasEval_profits (FleasEval *this);

/// Evalutarion result. Its value is between -1 and 1.
double fleasEval_eval (FleasEval *this);

///
Js *fleasEval_to_js (FleasEval *this);

///
FleasEval *fleasEval_from_js (Js *js);

/*--*/

/// Update fleas data.
void fleas_update (
  AsyncActor *ac,
  Qtable *opens,
  Qtable *closes,
  Fmodel *model,
  Opt *log_id // Opt[char]
);

/// Update fleas data in thread apart.
void fleas_run(AsyncActor *ac);

/// Returns Arr[FleasEval] with 'fleas' evaluated.
/// If 'filtered' != 0, fleas with sells out of bounds are removed.
Arr *fleas_evaluate (
  Qtable *opens,
  Qtable *closes,
  Fmodel *model,
  Arr *efleas, // Arr[FleasEval]
  int filtered
);

#endif
// Copyright 15-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Scheduler main.

#ifndef SCHEDULER_H
  #define SCHEDULER_H

#include "dmc/async.h"

///
void scheduler_run (AsyncActor *ac);

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array structure.

#ifndef DMC_ARR_H
  #define DMC_ARR_H

#include "DEFS.h"
#include "Tp.h"
#include "Tp3.h"

typedef struct js_Js Js;

typedef struct it_It It;

///
typedef struct arr_Arr Arr;

/// Creates a new Array with buffer size of 15 elements.
Arr *arr_new (void);

/// 'buffer' must be > 0.
Arr *arr_bf_new (int buffer);

/// Creates a new array from several elements.
Arr *arr_new_from (void *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
Arr *arr_new_c (int size, void **es);

/// Returns a new array with elements of 'this'.
Arr *arr_copy (Arr *this);

///
int arr_size (Arr *this);

/// Resturn the element at position ix.
void *arr_get (Arr *this, int ix);

/// Return a pointer to the first element of 'this'
void **arr_start (Arr *this);

/// Returns a pointer to the next element after the last element of 'this'.
/// 'arr_end' does not point to a valid element.
void **arr_end (Arr *this);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void arr_push (Arr *this, void *e);

/// Returns and removes the last element.
void *arr_pop (Arr *this);

/// Returns the las element.
void *arr_peek (Arr *this);

/// Sets the element at position ix.
void arr_set (Arr *this, int ix, void *e);

/// Inserts an element at position ix.
void arr_insert (Arr *this, int ix, void *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void arr_remove (Arr *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void arr_cat (Arr *this, Arr *other);

/// Inserts pointer to elements of 'other' at 'ix'
void arr_insert_arr (Arr *this, int ix, Arr *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void arr_remove_range (Arr *this, int begin, int end);

/// Removes every element of 'this'. Buffer size is equals to 15.
void arr_clear (Arr *this);

/// Removes every element of 'this'.
void arr_bf_clear (Arr *this, int buffer);

/// Reverses elements of 'this'.
void arr_reverse (Arr *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns 'true'
/// if 'e1' > 'e2'.
void arr_sort (Arr *this, int (*greater)(void *e1, void *e2));

/// arr_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void arr_shuffle (Arr *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int arr_all (Arr *this, int (*pred)(void *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int arr_any (Arr *this, int (*pred)(void *e));

/// Returns the index of the first elements which returns 'true'
/// with 'pred', or -1 if such element does not exist.
int arr_index (Arr *this, int (*pred)(void *e));

/// Returns the index of the last elements which returns 'true'
/// with 'pred', or -1 if such element does not exist.
int arr_last_index (Arr *this, int (*pred)(void *e));

/// arr_filter_in removes every element which returns 'false' with 'pred'.
void arr_filter_in (Arr *this, int (*pred)(void *e));

/// Returns a new Arr. See it_take.
Arr *arr_take (Arr *this, int n);

/// Returns a new Arr. See it_takef.
Arr *arr_takef (Arr *this, int (*predicate)(void *e));

/// Returns a new Arr. See it_drop.
Arr *arr_drop (Arr *this, int n);

/// Returns a new Arr. See it_dropf.
Arr *arr_dropf (Arr *this, int (*predicate)(void *e));

/// Returns a new Arr. See it_filter.
Arr *arr_filter_to (Arr *this, int (*predicate)(void *e));

/// Returns a new Arr. See it_map.
Arr *arr_map (Arr *this, void *(*converter)(void *e));

/// Returns a new Arr. See it_map2.
Arr *arr_map2 (Arr *this, void *(*conv1)(void *e), void *(*conv2)(void *e));

/// Returns a new Arr. Returns Arr[Tp]. See it_zip.
Arr *arr_zip (Arr *a1, Arr *a2);

/// Returns a new Arr. Returns Arr[Tp3]. See it_zip3.
Arr *arr_zip3 (Arr *a1, Arr *a2, Arr *a3);

/// Returns Tp[Arr, Arr] from an Arr[Tp]. 'Return_e1' contains elements of
/// source 'Tp_e1' and 'return_e2' elementso of 'Tp_e2'.
Tp *arr_unzip (Arr *this);

/// Returns Tp[Arr, Arr, Arr] from an Arr[Tp2]. 'Return_e1' contains elements
/// of source 'Tp_e1', 'return_e2' elements of 'Tp_e2' and 'return_e3'
/// elements of 'Tp_e3'.
Tp3 *arr_unzip3 (Arr *this);

/// Returns Tp[Arr, Arr] (duplicates, rest) See it_duplicates.
Tp *arr_duplicates (Arr *this, int (feq)(void *e1, void *e2));

/// Creates an iterator over 'this'.
It *arr_to_it (Arr *this);

/// Creates an Arr from 'it'.
Arr *arr_from_it (It *it);

/// Returns a Js from an element of 'this'
Js *arr_to_js (Arr *this, Js *(*to)(void *e));

/// Parses a Js to an element of 'this'.
Arr *arr_from_js (Js *js, void *(*from)(Js *jse));

#endif
// Copyright 29-May-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities to manage exceptions.
/// Only functions 'exc_init', 'exc_stack'  and 'exc_throw' must be used
/// directly. The rest must be used through the macros TRY-CATCH-_TRY and
/// THROW-_THROW. (See "?libdmc@dmc/DEFS#hp:TRY").

#ifndef DM_EXC_H
  #define DM_EXC_H

#include <setjmp.h>

typedef struct arr_Arr Arr;

///
typedef struct exc_Exc Exc;

/// Returns exception type. Predefined types are:
///   exc_generic_t
///   exc_range_t
///   exc_illegal_argument_t
///   exc_illegal_state_t
///   exc_io_t
char *exc_type (Exc *this);

/// Returns exception message.
char *exc_msg (Exc *this);

/// Returns Arr[char]. The exception stack trace.
Arr *exc_stack (Exc *this);

/// Intializes jumps buffer. This function has to be called before using macros
/// TRY-CATCH-FINALLY-_TRY or THROW-_THROW.
void exc_init ();

/// Initializes thread data. It is intended to beeng use only
/// by 'async_thread'.
void exc_thread_init (void);

/// Removes a thread data. It is intended to beeng use only by 'async_thread'.
void exc_thread_end (void);

/// Adds a exception to buffer of current Exc in current thread.
void exc_add (jmp_buf *jump);

/// Returns current Exc in current thread.
Exc *exc_get(void);

/// Removes the top of jumps buffer of current Exc in current thread.
void exc_remove ();

/// Sends an exception.
/// If no TRY block has been defined it stops the program.
///   type   : Excepion type.
///   message: Message to show.
///   file   : Error file.
///   func   : Error function.
///   line   : Error line number.
void exc_throw (
  char *type, char *message, char *file, char *func, int line
);

///
#define exc_generic_t "generic"

///
#define exc_range_t "range"

/// Exception for index out of range.
///   begin: Lower limit inclusive.
///   end  : Upper limit inclusive.
///   index: The index out of range (< begin and > end).
char *exc_range (int begin, int end, int index);

///
#define exc_illegal_argument_t "argument"

/// Exception for argument with a wrong value.
///   argument_name: Name of wrong argument.
///   expected     : Value expected.
///   actual       : Actual value.
char *exc_illegal_argument (char *argument_name, char *expected, char *actual);

///
#define exc_illegal_state_t "state"

/// Exception for attempting to use an object in wrong state.
///   cause: Description of problem.
char *exc_illegal_state (char *cause);

///
#define exc_io_t "io"

/// Exception for Input - Output error.
///   cause: Description of problem.
char *exc_io (char *cause);

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Structure for working with bytes. Example:
///   Bytes *bs = bytes_new();
///   const unsigned char bss[] = {0, 23, 116, 225};
///   bytes_add_bytes(bs, bss, 4);
///   char b41 = b41_encodeBytes(bs);
///   assert(!strcmp("RRoixx", b41));
///   bytes_free(bs);

#ifndef DMC_BYTES_H
  #define DMC_BYTES_H

#include "Js.h"

///
typedef struct bytes_Bytes Bytes;

///
Bytes *bytes_new(void);

///
Bytes *bytes_bf_new(int length);

/// Returns a new allocated 'Bytes' whitch is copy of 'bs'.
Bytes *bytes_from_bytes(unsigned char *bs, int length);

/// Returns a 'Bytes' whitch is copy of 's' without the ending zero.
Bytes *bytes_from_str(char *s);

///
unsigned char *bytes_bs(Bytes *this);

///
int bytes_len(Bytes *this);

/// Adds to 'this' a new copy of 'bs'.
void bytes_add_bytes(Bytes *this, unsigned char *bs, int length);

/// Adds to 'this' a new copy of 'another'.
void bytes_add(Bytes *this, Bytes *another);

/// Adds to 'this' a copy of 's' without the ending zero.
void bytes_add_str(Bytes *this, char *s);

///
Js *bytes_to_js(Bytes *this);

///
Bytes *bytes_from_js(Js *js);

#endif
// Copyright 13-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Regular expressions management.

#ifndef DMC_REGEX_H
  #define DMC_REGEX_H

#include "dmc/std.h"

/// Returns the offsets where ereg is found in s.
///   rex   : Regular expression to find. It does not admit grouping (that is,
///           parenthesis).
///   s     : String to search in.
///   return: Opt[Arr[Itp]]. Offsets of 's' with elements which match. Each
///           'Itp' contains [ofsset start inclusive - ofsect - end exclusive].
///           If any error happens, it returns opt_empty.
Opt *regex_matches (char *rex, char *s);

/// Equals to 'regex_matches' but "ignoring case".
Opt *regex_matches_ic (char *rex, char *s);

/// Replace ocurrences of 'rex' in 's' by 'replacement'. Ocurrences are
/// find by regex_matches.
///   rex        : Regular expression to replace. It does not admit grouping (that
///                is, parenthesis).
///   s          : String to search in.
///   replacement: New string.
///   return     : Opt[char]. A new string with repacements done.
///                If any error happens, it returns opt_empty.
Opt *regex_replace (char *rex, char *s, char *replacement);

/// Equals to regex_replace, but "ignoring case".
Opt *regex_replace_ic (char *rex, char *s, char *replacement);

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array of doubles.

#ifndef DMC_DARR_H
  #define DMC_DARR_H

#include "Js.h"

///
typedef struct darr_Darr Darr;

///
Darr *darr_new(void);

/// buffer must be > 0.
Darr *darr_bf_new(int buffer);

/// Creates a new array from a C array. For example:
///   Darr *a = darr_new_c(3, (double[]){1.0, 1.8, 1.3});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
Darr *darr_new_c (int size, double *es);

/// Returns a new Darr with elements from 0 to 'ix' (exclusive),
Darr *darr_left(Darr *this, int ix);

/// Returns a new Darr with elements from 'ix' (inclusive) to end of 'this'.
Darr *darr_right(Darr *this, int ix);

/// Returns a new Darr with elements from 'begin' (inclusive) to
/// to 'end' (exclusive),
Darr *darr_sub(Darr *this, int begin, int end);

///
Darr *darr_copy(Darr *this);

///
int darr_eq(Darr *this, Darr *other, double gap);

///
int darr_size(Darr *this);

/// If ix is < 0 then is changed to 'darr_size - ix'.
double darr_get(Darr *this, int ix);

///
double *darr_start(Darr *this);

///
double *darr_end(Darr *this);

///
void darr_push(Darr *this, double e);

/// If ix is < 0 then is changed to 'darr_size - ix'.
void darr_set(Darr *this, int ix, double e);

/// If ix is < 0 then is changed to 'darr_size - ix'.
void darr_insert(Darr *this, int ix, double e);

/// If ix is < 0 then is changed to 'darr_size - ix'.
void darr_remove(Darr *this, int ix);

///
void darr_cat(Darr *this, Darr *other);

/// If ix is < 0 then is changed to 'darr_size - ix'.
void darr_insert_arr(Darr *this, int ix, Darr *other);

/// If begin or end are < 0 then is changed to 'darr_size - itsValue'.
void darr_remove_range(Darr *this, int begin, int end);

/// Removes every element of 'this'. Buffer size is equals to 15.
void darr_clear (Darr *this);

/// Removes every element of 'this'.
void darr_bf_clear (Darr *this, int buffer);

///
void darr_reverse(Darr *this);

///
void darr_sort(Darr *this);

///
Js *darr_to_js(Darr *this);

///
Darr *darr_from_js(Js *js);

#endif
// Copyright 13-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tuple of two integers.

#ifndef DMC_ITP_H
  #define DMC_ITP_H

#include "dmc/std.h"

///
typedef struct itp_Itp Itp;

///
Itp *itp_new(int i1, int i2);

///
int itp_e1(Itp *this);

///
int itp_e2(Itp *this);

#endif
// Copyright 23-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Hash (immutable) structure.

#ifndef DM_HASH_H
  #define DM_HASH_H

#include "dmc/Arr.h"
#include "dmc/Opt.h"

///
typedef struct hash_Hash Hash;

/// Initializates a Hash. Hash can be cast to Map<Kv>.
Hash *hash_new(void);

/// Returns the number of elements. (O(n) operation).
int hash_count(Hash *this);

/// Puts 'value' with key 'key'. If key already exists, it will not
/// be deleted; but will not be used.
///   this : The Hash.
///   key  : Entry key.
///   value: New value
Hash *hash_put(Hash *this, char *key, void *value);

/// Returns 1 if 'this' has key 'key'.
int hash_has_key(Hash *this, char *key);

/// Returns the value pointed by key or 'opt_empty' if 'key' does.
/// not exist.
Opt *hash_get(Hash *this, char *key);

/// Returns a new hash with duplicates removed.
Hash *hash_compact(Hash *this);

/// Returns a new hash with the entry with key 'key' removed.
Hash *hash_remove(Hash *this, char *key);

/// Returns keys of this in a Arr[char] with duplicated keys removed.
Arr *hash_keys(Hash *this);

/// Returns an Arr<Kv> with dulicated keys removed.
Arr *hash_kvs(Hash *this);

/// Creates an iterator over 'this'. Duplicates are removed.
///   return: It<Kv>
It *hash_to_it(Hash *this);

/// Creates a Hash from a 'it'.
///   it: It<Kv>
Hash *hash_from_it(It *it);

/// Returns a Js from a value of 'this'. Duplicates are removed.
///   to: Value converter.
Js *hash_to_js(Hash *this, Js *(*to)(void *e));

/// Parses a Js to a value of 'this'.
///   from: Value converter.
Hash *hash_from_js(Js *js, void *(*from)(Js *jse));

#endif
// Copyright 23-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Pair key - value.

#ifndef DMC_KV_H
  #define DMC_KV_H

///
typedef struct kv_Kv Kv;

///
Kv *kv_new(char *key, void *value);

///
char *kv_key(Kv *this);

///
void *kv_value(Kv *this);

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Generator of random numbers.

#ifndef DMC_RND_H
  #define DMC_RND_H

#include "dmc/Arr.h"

///
typedef struct rnd_Box Box;

/// Intializates the random number generator.
void rnd_init(void);

/// Generates a new double between 0.0 (inclusive) and 1.0 (exclusive).
double rnd_d(void);

/// Generates a new int between 0 (inclusive) and 'top' (exclusive).
int rnd_i(int top);

/// Returns an 'Box' that iterates over 'a' elements randomly. When it finishes
/// with all the elements of 'a', restarts again.
Box *rnd_box_new(Arr *a);

/// Returns the next element of 'this'.
void *rnd_box_next(Box *this);

#endif
// Copyright 29-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of multithread programs.
/// NOTE: This file must be included instead of 'std.h'.

#ifndef DMC_ASYNC_H
  #define DMC_ASYNC_H

#include <pthread.h>
/// Necesary definition for multithreading garbage collector.
#define GC_THREADS
#include "gc.h"
#include "dmc/std.h"

/// Launchs 'fn' in a new joinable thread. Example of use:
///   void fn() { puts("Here"); }
///   pthread_t *thr = async_thread(fn);
///   async_join(thr); // Waits for thr.
/// NOTE: After calling 'async_thread' is mandatory to call 'async_join' to
///         free resources.
pthread_t *async_thread (void (*fn)(void));

/// Launchs 'fn' in a new joinable thread. Example of use:
///   void fn(char *tx) { puts(tx); }
///   pthread_t *thr = async_thread2((FPROC)fn, "Hello");
///   async_join(thr); // Waits for thr.
/// NOTE: After calling 'async_thread' is mandatory to call 'async_join' to
///         free resources.
pthread_t *async_thread2 (void (*fn)(void *), void *value);

/// Launch 'fn' in a new thread. Example of use:
///   void fn(char *tx) { puts(tx); }
///   async_thread_detached((FPROC)fn, "Hello");
void async_thread_detached (void (*fn)(void *), void *value);

/// Wait until thr finishes.
void async_join (pthread_t *thr);

///
typedef struct async_AsyncActor AsyncActor;

/// 'millis' is the latence time.
AsyncActor *asyncActor_new (int millis);

/// Executes 'fn(value)' synchronicaly. This function returns immediatly.
void asyncActor_run (AsyncActor *this, void (*fn)(void *), void *value);

/// Executes 'fn(value)' synchronicaly. This function stops the program
/// until 'fn' is finished.
void asyncActor_wait (AsyncActor *this, void (*fn)(void));

/// Finalizes 'this'. 'this' also will finish is pendant jobs.
void asyncActor_end (AsyncActor *this);

/// Waits until 'this' is finished.
void asyncActor_join (AsyncActor *this);

///
typedef struct async_AsyncTimer AsyncTimer;

/// Executes 'fn(value)' each 'millis' milliseconds.
AsyncTimer *asyncTimer_new (void (*fn)(void *), void *value, int millis);

/// Finalizes 'this'.
void asyncTimer_end (AsyncTimer *this);

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Cryptographic utilities.

#ifndef DMC_CRYP_H
  #define DMC_CRYP_H

/// Generates a b64 random key of a length 'lg'.
char *cryp_genk (int lg);

/// Codified 'k' in irreversible way, using 'lg' b64 digits..
///   k : String to codify.
///   lg: Length of result.
char *cryp_key (char *k, int lg);

/// Encodes 's' with key 'k'.
///   s: Message to encode.
///   k: Key for encoding.
char *cryp_cryp (char *s, char *k);

/// Decodes 'b64' using key 'k'. 'b64' was codified with cryp().
///   b64: Text codified with cryp().
///   k  : Key for decoding.
char *cryp_decryp (char *b64, char *k);

/// Encodes automatically 's' with a random key of 'nk' digits.
///   nK: Number of digits for random key (1 to 64 both inclusive).
///   s : Text for encoding.
char *cryp_auto_cryp (char *s, int nk);

/// Decodes a text codified with autoCryp().
///   b64: Codified text.
char *cryp_auto_decryp (char *b64);

/// Encodes 's' whith key 'k' and an autoKey of length 'nK'.
///   k : Key for encoding.
///   nk: Digits to generate autoKey (1 to 40 both inclusive).
///   s : Message to encode.
char *cryp_encode (char *s, int nk, char *k);

/// Decodes a string codified with encode()
///   b64: Message encoded with encode().
///   k  : Key for encoding.
char *cryp_decode (char *b64, char *k);

#endif
// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for HTML conections between client - server.

#ifndef DMC_CGI_H
  #define DMC_CGI_H

#include "dmc/std.h"

/// Returns of cgi_get_session_data.
typedef struct cgi_Session CgiSession;

/// Returns sesion user.
char *cgiSession_user(CgiSession *this);

/// Returns sesion user level.
char *cgiSession_level(CgiSession *this);

/// Returns comunication key.
char *cgiSession_key(CgiSession *this);

/// Returns the standard length of keys.
int cgi_klen(void);

/// Initializes a new interface of commnications.
///   home        : Aboslute path of application directory. For example:
///                   "/peter/wwwcgi/dmcgi/JsMon"
///                   or
///                   "/home/deme/.dmCApp/JsMon" ).
///   t_expiration: Time in seconds.
void cgi_init(char *home, time_t t_expiration);

/// Returns 'home' directory.
char *cgi_home(void);

/// Sets the key which 'cgi_ok' and 'cgi_err' will use. This function is called
/// when connection or authentication.
void cgi_set_key(char *k);

/// Returns Opt<CgiSession>.
/// If 'session_id' is not valid, it returns 'opt_empty'.
Opt *cgi_get_session(char *session_id);

/// Adds an user to database.
///   admin : Admin name.
///   akey  : Admin password.
///   user  : New user name.
///   ukey  : New user password.
///   level : New user level. Level "0" is the admin one.
///   return: A boolean field {ok:true|false}, sets to true if operations
///           succeeded. A fail can come up if 'admin' authentication fails.
char *cgi_add_user(
  char *admin, char *akey,
  char *user, char *ukey, char *level
);

/// Removea an user from database.
///   admin : Admin name.
///   akey  : Admin password.
///   user  : User name to remove.
///   return: A boolean field {ok:true|false}, sets to true if
///           operation succeeded. A fail can come up if 'admin' authentication
///           fails.
char *cgi_del_user(char *admin, char *akey, char *user);

/// Modifies user level.
///   admin : Admin name.
///   akey  : Admin password.
///   user  : User name to change level.
///   level : New user level. Level "0" is the admin one.
///   return: A boolean field {ok:true|false}, sets to true if
///           operation succeeded. A fail can come up if 'admin' authentication
///           fails or 'user' does not exist.
char *cgi_change_level(
  char *admin, char *akey, char *user, char *level
);

/// Changes user password.
///   user   : User name to change password.
///   key    : Old password.
///   new_key: New password.
///   return : A boolean field {ok:true|false}, sets to true if operation
///            succeeded. A fail can come up if 'user' authentication fails.
char *cgi_change_pass(char *user, char *key, char *new_key);

/// cgi_del_session deletes 'session' and returns an empty response.
char *cgi_del_session(char *session_id);

/// Sends to client level, key, page_id and session_id.
/// If authentication failed every value is "".
///   user      : User name.
///   key       : User password.
///   expiration: If is set to false, session will expire after 30 days.
///   return    : 'level', 'key', 'pageId' and 'sessionId' fields.
char *cgi_authentication(char *user, char *key, int expiration);

/// Returns client 'connectionId' and 'key'. If conection failed both are "".
///   session_id: Session identifier.
///   return    : {connectionId: String, key: String}.
///               'key' is a new key, set for the new connection.
char *cgi_connect(char  *session_id);

/// Returns a normal response.
/// 'data' is a Map[Js]
char *cgi_ok(Map *data);

/// Retuns an empty response.
char *cgi_empty(void);

/// Returns an error response, setting {error:msg}.
char *cgi_error(char *msg);

/// Returns a expired response, setting {expired:1}.
char *cgi_expired(void);

/// Runs a "long run" task. This function is intended to be called until it
/// returns {"longRunEnd"='true'}.
///   fn    : Map[Js] *(*)(void *ctx, Map[Js *rq]). "Long run" task. <i>It must
///           not be defined as inner function</i>.
///   ctx   : Context. It can be NULL. Value to pass to fn.
///   rq    : Map[js]. Data for 'fn'. 'rq' must have a field called
///           "longRunFile" which value the first time it is called is "" and
///           after
///           that its value is the returned by this function.
///   return:
///     first call     : A Map[Js] with an only field "longRunFile" whitch must
///                      be used in following calls.
///     following calls: - If 'fn' was finished the Map returned with 'fn' with
///                        the field {"longRunEnd"='true'} added.
///                      - If 'fn' is running a Map with the only field
///                        {"longRunEnd"='false'}
Map *cgi_long_run(Map *(*fn)(void *ctx, Map *rq), void *ctx, Map *rq);
#endif
// Copyright 30-May-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Iterator.

#ifndef DM_IT_H
  #define DM_IT_H

#include <stddef.h>
#include "Arr.h"

typedef struct opt_Opt Opt;

///
typedef struct it_It It;

///
typedef Opt *(*it_Next)(void *);

/// it_new creates a new It.
///   o   : Container
///   next: Function which returns a element of 'o' or Opt_empty() if there
///         are no more elements.
It *it_new (void *o, Opt *(*next)(void *o));

///
It *it_empty (void);

///
It *it_unary (void *e);

/// it_range is an iterator that returns values between begin (inclusive)
/// and end (exclusive).
It *it_range (int begin, int end);

/// it_range0 is equals to it_range(0, end).
It *it_range0 (int end);

///
int it_has_next (It *this);

///
void *it_next (It *this);

/// Shows next element witout advancing.
void *it_peek (It *this);

/// it_add adds an element at the end of 'this's
It *it_add (It *this, void *element);

/// it_add adds an element at the beginning of 'this's
It *it_add0 (It *this, void *element);

///
It *it_cat (It *this, It *another);

///
It *it_take (It *this, size_t n);

///
It *it_takef (It *this, int (*predicate)(void *e));

///
It *it_drop (It *this, size_t n);

///
It *it_dropf (It *this, int (*predicate)(void *e));

///
It *it_filter (It *this, int (*predicate)(void *e));

///
It *it_map (It *this, void *(*converter)(void *e));

/// it_map2 applies conv1 to the first element and conv2 to the others.
It *it_map2 (It *this, void *(*conv1)(void *e), void *(*conv2)(void *e));

/// Returns It<Tp>.
It *it_zip (It *it1, It *it2);

/// Returns It<Tp3>.
It *it_zip3 (It *it1, It *it2, It *it3);

///
It *it_reverse (It *this);

/// it_sort sorts 'this' calling 'arr_sort'.
It *it_sort (It *this, int (*greater)(void *e1, void *e2));

///
void it_each (It *this, void (*f)(void *e));

///
void it_each_ix (It *this, void (*f)(void *e, size_t ix));

///
size_t it_count (It *this);

///
int it_eq (It *it1, It *it2, int (*feq)(void *e1, void *e2));

///
int it_index (It *this, int (*predicate)(void *e));

///
int it_contains (It *this, int (*predicate)(void *e));

///
int it_last_index (It *this, int (*predicate)(void *e));

/// Returns the first element which satisfies 'predicate' or opt_empty().
Opt *it_find (It *this, int (*predicate)(void *e));

/// Returns the first element which satisfies 'predicate' or 'option'.
void *it_ofind (It *this, void *option, int (*predicate)(void *e));

/// Creates an array from 'this'.
Arr *it_to (It *this);

/// Creates an It from 'a'.
It *it_from (Arr *a);

/// Returns Tp<Arr, Arr> (dup, rest): 'dup' with duplicates values (only the
/// first case) and 'rest' with every element of 'this' without duplicates.
Tp *it_duplicates (It *this, int (feq)(void *e1, void *e2));

#endif


// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Functions using external programs.

#ifndef DMC_EXT_H
  #define DMC_EXT_H

/// Calls "wget -q -O - 'url'" and returns the text read.
/// If the reading fails, it returns an empty string.
char *ext_wget (char *url);

/// Reads an URL.
/// Calls "node -e [script] 2>/dev/null" and returs the text read.
/// If the reading fails, it returns an empty string.
/// [script] is a script which run a node library called "puppeteer". This
/// library must be downloaded (npm install puppeteer).
///   url: URL to read. It must include protocol ("https://www.google.com").
char *ext_puppeteer (char *url);

/// Reads a text using GUI. It calls:
///   zenity --entry --title='title' --text='prompt' 2>/dev/null
/// The return removes starting and trailing spaces.
/// If user clicks on cancel, it returns an empty string.
/// It is posible set a default text adding in promp:
///   \" --entry-text \"[text_to_add]
char *ext_zenity_entry (char *title, char *prompt);

/// ext_zenity_msg shows a message box. It calls:
///   zenity --notification --window-icon='icon' --text='text' 2>/dev/null
/// 'icon' is one of gnome icon stock. For example: info, dialog-warning,
/// dialog-error, dialog-information, face-wink, etc
void ext_zenity_msg (char *icon, char *text);

/// ext_pdf generates a pdf file from a html text. It calls:
///   pdfPrinter -s %s -t %s 'options' 2>&1
///
///   tx_source  : Text html
///   file_target: Path of the new pdf file
///   options    : Options for pdfPrinter
void ext_pdf (
  char *tx_source,
  char *file_target,
  char *options
);

/// ext_zip compress source in target. It calls:
///   zip -q 'target' 'source' 2>&1
/// if 'target' already exists, source will be added to it. If you require a
/// fresh target file, you have to delete it previously.
///   source: can be a file or directory,
///   target: Zip file. If it is a relative path, it hangs on source parent.
void ext_zip (char *source, char *target);

/// ext_unzip uncompress source in target, It calls:
///   unzip -q 'source' -d 'target' 2>&1
///
///   source: Zip file.
///   target: A directory. It it does not exist, it is created.
void ext_unzip (char *source, char *target);

#endif
// Copyright 23-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tuple of three values.

#ifndef DMC_TP3_H
  #define DMC_TP3_H

///
typedef struct tp3_Tp3 Tp3;

///
Tp3 *tp3_new(void *e1, void *e2, void *e3);

///
void *tp3_e1(Tp3 *this);

///
void *tp3_e2(Tp3 *this);

///
void *tp3_e3(Tp3 *this);

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing file paths.

#ifndef DMC_PATH_H
  #define DMC_PATH_H

#include "Opt.h"

/// Returns name and extension of path.
/// If path is "" or ends at ("/") it returns "".
char *path_name (char *path);

/// Returns the parent path of 'path'.
/// If 'path' is "/" or a string without '/', it returns an empty string.
char *path_parent (char *path);

/// Returns only extension of path. Extension is returned with point,
/// (e.g., ".", ".txt").
/// If path does not have extension it returns "".
char *path_extension (char *path);

/// Returns only name of path.
/// If path is "", ends at ("/"), or if file starts with point, it returns "".
char *path_only_name (char *path);

/// Concatenates paths. Variable argumens must finish with NULL.
char *path_cat (char *s, char *more, ...);

/// Returns Opt<char> Cannonical representation of 'path'.
///   - If some component of 'path' is not in file system, returns 'opt_empty'.
///   - Directories do not finish in '/'.
Opt *path_canonical (char *s);

#endif
// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing dates.

#ifndef DMC_DATE_H
  #define DMC_DATE_H

#include <time.h>
#include "dmc/std.h"

/// Makes a new time_t.
///   this : New time_t.
///   year : Year with all digits.
///   month: Month in base 1 (1 to 12).
///   day  : Day in base 1 (1 to 31).
time_t date_new (int day, int month, int year);

/// Returns the current date.
time_t date_now (void);

/// Makes a date from a string is in format 'yyyymmdd'.
/// (month and day in base 1).
time_t date_from_str (char *date);

/// Makes a date from a string in format '[x]x/[x]x/[xxx]x'.
/// (month and day in base 1).
time_t date_from_iso (char *date);

/// Mmakes a date from a string in format '[x]x/[x]x/[xxx]x'.
/// (month and day in base 1).
time_t date_from_us (char *date);

/// Makes a date from a string is in format '[x]xSP[x]xSP[xxx]x'.
/// If 'data' is not valid, returns '0'.
time_t date_from_iso_sep (char *date, char sep);

/// Makes a date from a string in format '[x]xSP[x]xSP[xxx]x'.
/// If 'data' is not valid, returns '0'.
time_t date_from_us_sep (char *date, char sep);

///
int date_eq (time_t this, time_t another);

///
int date_cmp (time_t this, time_t another);

/// Returns the difference in days this - another.
int date_df (time_t this, time_t another);

///
time_t date_add (time_t this, int days);

///
int date_day (time_t this);

///
int date_month (time_t this);

///
int date_year (time_t this);

/// Formats a time_t. Format can be:
///   %a     The abbreviated name of the day of the week according to the
///          current locale.
///   %A     The full name of the day of the week according to the current
///          locale.
///   %b     The abbreviated month name according to the current locale.
///   %B     The full month name according to the current locale.
///   %c     The preferred date and time representation for the current
///          locale.
///   %C     The century number (year/100) as a 2-digit integer. (SU)
///   %d     The day of the month as a decimal number (range 01 to 31).
///   %D     Equivalent to %m/%d/%y.  (Yecch—for Americans only.  Americans
///          should note that in other countries %d/%m/%y is rather common.
///          This means that in international context this format is
///          ambiguous and should not be used.) (SU).
///   %e     Like %d, the day of the month as a decimal number, but a
///          leading zero is replaced by a space. (SU).
///   %F     Equivalent to %Y-%m-%d (the ISO 8601 date format). (C99).
///   %G     The ISO 8601 week-based year (see NOTES) with century as a
///          decimal number.  The 4-digit year corresponding to the ISO
///          week number (see %V).  This has the same format and value as
///          %Y, except that if the ISO week number belongs to the previous
///          or next year, that year is used instead. (TZ).
///   %g     Like %G, but without century, that is, with a 2-digit year
///          (00-99). (TZ).
///   %h     Equivalent to %b.  (SU).
///   %H     The hour as a decimal number using a 24-hour clock (range 00
///          to 23).
///   %I     The hour as a decimal number using a 12-hour clock (range 01
///          to 12).
///   %j     The day of the year as a decimal number (range 001 to 366).
///   %k     The hour (24-hour clock) as a decimal number (range 0 to 23);
///          single digits are preceded by a blank.  (See also %H.)  (TZ)
///   %l     The hour (12-hour clock) as a decimal number (range 1 to 12);
///          single digits are preceded by a blank.  (See also %I.)  (TZ)
///   %m     The month as a decimal number (range 01 to 12).
///   %M     The minute as a decimal number (range 00 to 59).
///   %n     A newline character. (SU)
///   %O     Modifier: use alternative format, see below. (SU)
///   %p     Either "AM" or "PM" according to the given time value, or the
///          corresponding strings for the current locale.  Noon is treated
///          as "PM" and midnight as "AM".
///   %P     Like %p but in lowercase: "am" or "pm" or a corresponding
///          string for the current locale. (GNU)
///   %r     The time in a.m. or p.m. notation.  In the POSIX locale this
///          is equivalent to %I:%M:%S %p.  (SU)
///   %R     The time in 24-hour notation (%H:%M).  (SU) For a version
///          including the seconds, see %T below.
///   %s     The number of seconds since the Epoch, 1970-01-01 00:00:00
///          +0000 (UTC). (TZ)
///   %S     The second as a decimal number (range 00 to 60).  (The range
///          is up to 60 to allow for occasional leap seconds).
///   %t     A tab character. (SU).
///   %T     The time in 24-hour notation (%H:%M:%S). (SU).
///   %u     The day of the week as a decimal, range 1 to 7, Monday being
///          1.  See also %w.  (SU)
///   %U     The week number of the current year as a decimal number, range
///          00 to 53, starting with the first Sunday as the first day of
///          week 01.  See also %V and %W.
///   %V     The ISO 8601 week number (see NOTES) of the current year as a
///          decimal number, range 01 to 53, where week 1 is the first week
///          that has at least 4 days in the new year.  See also %U and %W.
///          (SU).
///   %w     The day of the week as a decimal, range 0 to 6, Sunday being
///          0.  See also %u.
///   %W     The week number of the current year as a decimal number, range
///          00 to 53, starting with the first Monday as the first day of
///          week 01.
///   %x     The preferred date representation for the current locale
///          without the time.
///   %X     The preferred time representation for the current locale
///          without the date.
///   %y     The year as a decimal number without a century (range 00 to
///          99).
///   %Y     The year as a decimal number including the century.
///   %z     The +hhmm or -hhmm numeric timezone (that is, the hour and
///          minute offset from UTC). (SU).
///   %Z     The timezone name or abbreviation.
///   %%     A literal '%' character.
char *date_f (time_t this, char *template);

/// Returns a string in format 'yyyymmdd'.
char *date_to_str (time_t this);

/// Returns a string in format 'dd/mm/yyyy'.
char *date_to_iso (time_t this);

/// Returns a string in format 'mm/dd/yyyy'.
char *date_to_us (time_t this);

///
Js *date_to_js (time_t this);

///
time_t date_from_js (Js *js);

///
typedef struct timeval DateTm;

/// Returns the current time with microsconds of precission.
///   DateTm (struct timeval) has following fields:
///     time_t tv_sec - Seconds.
///     long int tv_usec - Microseconds (0 - 999999).
DateTm *dateTm_now ();

/// Returns t1 - t2.
DateTm *dateTm_tdf (DateTm *t1, DateTm *t2);

/// Adds 'millis' milliseconds to 't'. 'millis' can be negative.
DateTm *dateTm_add (DateTm *t, int millis);

/// Returns t1 - t2 in milliseconds.
int dateTm_df (DateTm *t1, DateTm *t2);

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

///String utilities.

#ifndef DMC_STR_H
  #define DMC_STR_H

#include <stdarg.h>
#include <string.h>
#include "Opt.h"
#include "Arr.h"

/// Returns a copy of 'str'.
char *str_new(char *s);

/// Returns a string with the character 'ch'.
char *str_c(char ch);

/// Returns 'strcoll(s1, s2)'.
int str_cmp_locale(char *s1, char *s2);

/// Returns 'strcmp(s1, s2) > 0'.
int str_greater(char *s1, char *s2);

/// Returns 'strcoll(s1, s2) > 0'.
int str_greater_locale(char *s1, char *s2);

/// Returns 'true' if 'str1 == str2'.
int str_eq(char *str1, char *str2);

/// Indicates if 'str' starts with 'substr'.
int str_starts(char *str, char *substr);

/// Indicates if 'str' ends with 'substr'.
int str_ends(char *str, char *substr);

/// Returns first position of 'ch' in 'str' or -1 if 'ch' is missing.
int str_cindex(char *str, char ch);

/// Returns first position of 'ch' in 'str' from start or -1 if 'ch' is missing.
int str_cindex_from(char *str, char ch, int start);

/// Returns first position of 'substr' in 'str' or -1 if 'substr' is missing.
int str_index(char *str, char *substr);

/// Returns first position of 'substr' in 'str' from start or -1 if 'substr' is
/// missing.
int str_index_from(char *str, char *substr, int start);

/// Returns last position of 'ch' in 'str' or -1 if 'ch' is missing.
int str_last_cindex(char *str, char ch);

/// Returns last position of 'substr' in 'str' or -1 if 'substr' is missing.
int str_last_index(char *str, char *substr);

/// str_cat is a string concatenation.
/// Variable argumens must finish with NULL.
char *str_cat(char *s, ...);

/// Returns a substring.
///   - If 'begin' or 'end' are negatives, they are subtracted from 'strlen(s)'.
///   - If 'begin' or 'end' are out of bounds, function throws a RANGE exception.
char *str_sub(char *str, int begin, int end);

/// str_left is equals to 'str_sub(str, 0, end)'.
char *str_left(char *str, int end);

/// str_right is equals to 'str_sub(str, begin, strlen(s))'.
char *str_right(char *str, int begin);

/// Returns a new string in reverse order
char *str_reverse(char *str);

/// Returns a new string removing spaces (ch <= ' ') at left.
char *str_ltrim(char *str);

/// Returns a new string removing spaces (ch <= ' ') at right.
char *str_rtrim(char *str);

/// Returns a new string removing spaces (ch <= ' ') at left and right.
char *str_trim(char *str);

/// Splits 'str' in an Arr[char].
/// For example (using ';' as separator):
///   "" -> [""]
///   ";" -> ["", ""]
///   "a;" -> [a, ""]
///   "a;bc" -> ["a", "bc"]
/// Returns an Arr<char>.
Arr *str_csplit(char *str, char sep);

/// str_csplit_trim is similar to <tt>str_csplit</tt> but trimming elements.
/// Returns an Arr<char>.
Arr *str_csplit_trim(char *str, char sep);

/// Splits 'str' in an Arr<char>.
/// For example (using ";" as separator):
///   "" -> [""]
///   ";" -> ["", ""]
///   "a;" -> [a, ""]
///   "a;bc" -> ["a", "bc"]
/// If 'sep' is "" return all runes of 'str'.
/// Returns an Arr<char>.
Arr *str_split(char *str, char *sep);

/// str_split_trim is similar to <tt>str_split</tt> but trimming elements.
/// Returns an Arr[char]
Arr *str_split_trim(char *str, char *sep);

/// Joins elements of 'a', separated by 'sep'.
/// 'a' is Arr<char>.
char *str_cjoin(Arr *a, char sep);

/// Joins elements of 'a', separated by 'sep'.
/// 'a' is Arr<char>.
char *str_join(Arr *a, char *sep);

/// Returns a new string replacing 'old' by 'new' in 's'.
char *str_creplace(char *s, char old, char new);

/// Returns a new string replacing 'old' by 'new' in 's'.
/// If 'old' is "", it does nothing.
char *str_replace(char *s, char *old, char *new);

/// Returns a string with format similar to 'vprintf'.
char *str_vf(char *format, va_list args);

/// Returns a string with format similar to 'printf'.
char *str_f(char *format, ...);

/// Returns utf8 caracter number or -1 if 's' is not a valid utf8 string.
int str_runes(char *s);

/// Reads next rune of 's'.
/// If there are no more runes or it fails, 'rune' is equals to "".
/// Example:
///   char *tx = "a text";
///   char *rune;
///   char *rest = str_next_rune(&rune, tx);
///   while (*rune) {
///     puts(rune);
///     rest = str_next_rune(&rune, rest);
///   }
char *str_next_rune(char **rune, char *s);

/// Codifies a string in Unicode. Returns an 'Opt_empty()' if there are errors.
///   return: Opt<unsigned>
Opt *str_to_unicode(char *s);

/// Decodifies a string from Unicode. Returns an 'Opt_empty()' if there are
/// errors.
///   return: Opt<char>
Opt *str_from_unicode(unsigned *u);

/// Decodifies a string from ISO-8859-1.
char *str_from_iso(char *s);

/// Returns 's' converted to uppercase. It is necessary set language previously.
/// For example:
///   sys_locale("es_ES.utf8");
///   char *s = str_to_upper_new("cañón");
///   puts(s);
/// This function can fail if s is not a valid utf-8 string.
char *str_to_upper (char *s);

/// Returns 's' converted to lowercase. It is necessary set language previously.
/// For example:
///   sys_locale("es_ES.utf8");
///   char *s = str_to_lower_new("cañón");
///   puts(s);
/// This function can fail if s is not a valid utf-8 string.
char *str_to_lower (char *s);

/// Replaces " by \" and \ by \\ and insert the result inside quotes.
char *str_to_escape (char *s);

/// Restores the string escaped with 'escape'. If 's' does not come from
/// 'escape' the result is indefined.
char *str_from_escape (char *s);

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Map structure.

#ifndef DMC_MAP_H
  #define DMC_MAP_H

#include "dmc/Arr.h"

typedef struct opt_Opt Opt;

///
typedef struct map_Map Map;

/// Initializates a map. Map can be cast to Arr<Kv>.
Map *map_new(void);

///
int map_size(Map *this);

/// Puts 'value' with key 'key'. If key already exists its value
/// is changed.
///   this : The map.
///   key  : Entry key.
///   value: New value
void map_put(Map *this, char *key, void *value);

/// Returns 1 if 'this' has key 'key'.
int map_has_key(Map *this, char *key);

/// Returns the value pointed by key or 'opt_empty' if 'key' does not exist.
Opt *map_get(Map *this, char *key);

/// Removes value with key 'key' or does nothing if 'key' does not exist.
void map_remove(Map *this, char *key);

/// Returns keys of this in a Arr<char>.
Arr *map_keys(Map *this);

/// Equals to (Arr *)this
/// Returns an Arr[Kv]
Arr *map_kvs(Map *this);

/// Sorts 'this' from keys.
void map_sort(Map *this);

/// Sorts 'this' in locale from keys.
void map_sort_locale(Map *this);

/// Creates an iterator over 'this'.
///   return: It<Kv>
It *map_to_it(Map *this);

/// Creates a Map from 'it'
///   it: It[Kv]
Map *map_from_it(It *it);

/// Returns a Js from a value of 'this'.
///   to: Value converter.
Js *map_to_js(Map *this, Js *(*to)(void *e));

/// Parses a Js to a value of 'this'.
///   from: Value converter.
Map *map_from_js(Js *js, void *(*from)(Js *jse));

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Definitions.

#ifndef DMC_DEFS_H
  #define DMC_DEFS_H

typedef struct js_Js Js;

typedef struct schd_Task SchdTask;

///
#define MALLOC(type) (type *)GC_MALLOC(sizeof(type))

///
#define ATOMIC(size) GC_MALLOC_ATOMIC(size)

///
#define REPEAT(n) { \
  int __i = (n) + 1; \
  while (--__i) {

///
#define _REPEAT }}

///
#define RANGE0(i, end) { \
  int __end = end; \
  int i = -1; \
  while (++i < __end) {

///
#define RANGE(i, begin, end) { \
  int __end = end; \
  int i = (begin) - 1; \
  while (++i < __end) {

///
#define _RANGE }}

/// Iterates over an Iarr. See EACH.
#define IEACH(a, n) { \
  Iarr *__a = a; \
  int *__p = iarr_start(__a); \
  int *__pend = iarr_end(__a); \
  int n; \
  while(__p < __pend) { \
    n = *__p++;

/// Iterates over an Iarr. See EACH_IX.
#define IEACH_IX(a, n, ix) { \
  Iarr *__a = a; \
  int *__p = iarr_start(__a); \
  int *__pend = iarr_end(__a); \
  int n; \
  int ix = -1; \
  while(__p < __pend) { \
    ++ix; \
    n = *__p++;

/// Iterates over an Darr. See EACH.
#define DEACH(a, n) { \
  Darr *__a = a; \
  double *__p = darr_start(__a); \
  double *__pend = darr_end(__a); \
  double n; \
  while(__p < __pend) { \
    n = *__p++;

/// Iterates over an Darr. See EACH_IX
#define DEACH_IX(a, n, ix) { \
  Darr *__a = a; \
  double *__p = darr_start(__a); \
  double *__pend = darr_end(__a); \
  double n; \
  int ix = -1; \
  while(__p < __pend) { \
    ++ix; \
    n = *__p++;

/// Iterates over an 'Arr'. You can access to the 'element' index with _i.
/// For example:
///   EACH(a, char, s) {
///     printf("[%d] -> %s\n", _i, s);
///   } _EACH
///
///   a      : An Arr *.
///   type   : Element type without pointer sign (*).
///   element: An element of type 'type'.
#define EACH(a, type, element) { \
  Arr *__arr = (Arr *)a; \
  size_t __size = arr_size(__arr); \
  size_t _i; \
  type *element; \
  for (_i = 0; _i < __size; ++_i) { \
    element = arr_get(__arr, _i);

/// Iterates over an 'Arr'. You can access to the 'element' index with i.
/// For example:
///   EACH(a, char, s, i) {
///     printf("[%d] -> %s\n", i, s);
///   } _EACH
///
///   a      : An Arr *.
///   type   : Element type without pointer sign (*).
///   e: An element of type 'type'.
///   i: Element index.
#define EACH_IX(a, type, e, i) { \
  Arr *__a = (Arr *)a; \
  void **__p = arr_start(__a); \
  void **__pend = arr_end(__a); \
  type *e; \
  int i = -1; \
  while(__p < __pend) { \
    ++i; \
    e = *__p++;

/// Iterates over an 'It'. You can access to the 'element' index with _i.
/// For example:
///   EACH(it, char, s) {
///     printf("[%d] -> %s\n", _i, s);
///   } _EACH
///
///   a      : An It *.
///   type   : Element type without pointer sign (*).
///   element: An element of type 'type'.
#define EACHI(it, type, element) { \
  It *__it = (It *)it; \
  int _i = -1; \
  type *element; \
  while (it_has_next(__it)) { \
    ++_i; \
    element = it_next(__it);

/// Iterates over a 'List'.
/// For example:
///   EACHL(list, char, s) {
///     printf("%s\n", s);
///   } _EACH
///
///   list   : An List *,
///   type   : Element type without pointer sign (*),
///   element: An element of type 'type',
#define EACHL(list, type, element) { \
  List *_EACHL_list = (List *)list; \
  type *element; \
  while (!list_empty(_EACHL_list)) { \
    element = list_head(_EACHL_list); \
    _EACHL_list = list_tail(_EACHL_list);

/// Iterates over an 'Arr' in reverse order. You can access to the 'element'
/// index with _i.
/// For example:
///   EACHR(a, char, s) {
///     printf("[%d] -> %s\n", _i, s);
///   } _EACH
///
///   a      : An Arr *.
///   type   : Element type without pointer sign (*).
///   element: An element of type 'a'.
#define EACHR(a, type, element) { \
  Arr *__arr = (Arr *)a; \
  size_t _i = arr_size(__arr); \
  type *element; \
  while (_i) { \
    element = arr_get(__arr, --_i);

/// Finalizes an EACHL, EACH or a EACHR.
#define _EACH }}

///
typedef void *(*FCOPY)(void *);

///
typedef int (*FCMP)(void *, void *);

///
typedef void (*FPROC)(void *);

///
typedef int (*FPRED)(void *);

///
typedef Js *(*FTO)(void *);

///
typedef void *(*FFROM)(Js *);

///
typedef void (*FLOOP)(void *, SchdTask *);

/// Macros to manage exceptions. Example:
///   TRY
///     ...
///   CATCH (e)
///     puts(exc_msg(e));
///   _TRY
/// NOTE: CATCH block must return 'void'
#define TRY { \
  jmp_buf *_TRY_buf = MALLOC(jmp_buf); \
  exc_add(_TRY_buf); \
  if (!setjmp(*_TRY_buf)) { \

/// See TRY.
#define CATCH(e) ;exc_remove();} else { Exc *e = exc_get();

/// See>TRY.
#define _TRY ;exc_remove();}}

/// Example:
///   THROW(exc_io_t) "Working directory not found: %s", strerror(errno) _THROW
#define THROW(type) exc_throw(type, str_f(

///
#define _THROW ), __FILE__, (char *)__func__, __LINE__);

/// Example:
///   EXC_GENERIC("Fail")
#define EXC_GENERIC(msg) \
  THROW(exc_generic_t) msg _THROW

/// Throw a range exception if v < 'min' or v > 'max'.
/// Example:
///   EXC_RANGE(v, 0, 23) // -1 and 24 throw exeption.
#define EXC_RANGE(value, min, max) { \
    int __v = value; \
    if (__v < (min) || __v > (max)) \
      THROW(exc_range_t) exc_range((min), (max), __v) _THROW \
  }

/// Example:
///   EXC_ILLEGAL_ARGUMENT("Fail", "a value", "another value")
#define EXC_ILLEGAL_ARGUMENT(msg, expected, actual) \
  THROW(exc_illegal_argument_t) \
    exc_illegal_argument(msg, expected, actual) \
  _THROW

/// Example:
///   EXC_ILLEGAL_STATE("Fail")
#define EXC_ILLEGAL_STATE(msg) \
  THROW(exc_illegal_state_t) exc_illegal_state(msg) _THROW

/// Example:
///   EXC_IO("Fail")
#define EXC_IO(msg) \
  THROW(exc_io_t) exc_io(msg) _THROW

/// Reads a 'field' of 'map'. If 'field' is not found produce an
/// ILLEGAL_ARGUMENT exception, otherwise returns its value in 'type var'
/// using 'fun'.
/// Examples:
///   CGI_GET(int, index, js_ri, m)
///   CGI_GET(char *, value, js_rs, m)
///   CGI_GET(Arr *, values, js_ra, m)
///
///   type: type of var.
///   var : name of variable.
///   fun : function to pass 'Js' to 'type'.
///   map : A Map<Js>.
#define CGI_GET(type, var, fun, map) \
  type var; \
  { \
    Opt *js = map_get(map, #var); \
    if (opt_is_empty(js))  \
      EXC_ILLEGAL_ARGUMENT(#var, "Map key", "Key not found") \
    var = fun(opt_get(js)); \
  }

/// Calls CGI_GET with 'var' as 'int'.
#define CGI_GET_BOOL(var, map) \
  CGI_GET(int, var, js_rb, map)

/// Calls CGI_GET with 'var' as 'int'.
#define CGI_GET_INT(var, map) \
  CGI_GET(int, var, js_ri, map)

/// Calls CGI_GET with 'var' as 'double'.
#define CGI_GET_DOUBLE(var, map) \
  CGI_GET(double, var, js_rd, map)

/// Calls CGI_GET with 'var' as 'char *'.
#define CGI_GET_STR(var, map) \
  CGI_GET(char *, var, js_rs, map)

/// Calls CGI_GET with 'var' as 'Arr<Js>'.
#define CGI_GET_ARR(var, map) \
  CGI_GET(Arr *, var, js_ra, map)

/// Calls CGI_GET with 'var' as 'Map<Js>'.
#define CGI_GET_MAP(var, map) \
  CGI_GET(Map *, var, js_ro, map)

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing files.

#ifndef DMC_FILE_H
  #define DMC_FILE_H

#include <time.h>
#include "dmc/Arr.h"
#include "dmc/Bytes.h"

///
typedef struct file_FileLck FileLck;

/// Returns a new file path whose template is:
/// /tmp/'prefix'xxxxxxxxxx. Where xxxxxxxxxx is an aleatory sequence of
/// digits.
char *file_tmp (char *prefix);

/// Returns a new file path whose template is:
/// 'dir'/'prefix'xxxxxxxxxx. Where xxxxxxxxxx is an aleatory sequence of
/// digits.
char *file_tmp_in (char *dir, char *prefix);

/// Returns the working directory
char *file_cwd (void);

/// Sets the working directory
void file_cd (char *path);

/// Makes a directory with 0755 permissions.
/// If parent directory does not exist it creates it.
/// If 'path' already exists it does nothing.
void file_mkdir (char *path);

/// fReturns an Arr[char] with names of files and directories existing in
/// 'path'.
/// Values '.' and '..' are not in the return.
Arr *file_dir (char *path);

/// Deletes file or directory named 'path' although it is a not empty directory.
/// If 'path' does not exists it does nothing.
void file_del (char *path);

/// Renames 'old_path' to 'new_path'.
void file_rename (char *old_path, char *new_path);

/// Makes a symbol link from 'new_path' to 'old_path'.
void file_link (char *old_path, char *new_path);

/// file_exits returns true if 'path' exists in the file system.
int file_exists (char *path);

/// Returns true if file is a directory
int file_is_directory (char *path);

/// Returns information of 'path'. If path does no exist or another error
/// happens, this function produces a exception.
/// Some fields to check in 'struct stat' are:
///   mode_t st_mode  - It can be tested with S_ISDIR(), S_ISREG or S_ISLNK
///                     among others. This macros are defined in sys/stat.h
///   uid_t st_uid    - User id
///   gid_t st_gid    - Group id
///   off_t st_size   - File size
///   time_t st_atime - Last access to file in seconds.
///   time_t st_mtime - Last file modification in seconds.
struct stat *file_info (char *path);

/// Returns the size of 'path'.
int file_size(char *path);

/// Returns the last access in seconds.
time_t file_modified(char *path);

/// reads data from 'path', including ends of line.
/// This function opens, reads and closes file.
char *file_read (char *path);

/// Writes 'data' on 'path'.
/// This function opens, writes and closes file.
void file_write (char *path, char *text);

/// Appends 'data' on 'path'.
/// This function opens, writes and closes file.
void file_append (char *path, char *text);

/// Binary copy source to target.
void file_copy (char *source_path, char *target_path);

/// Opens a file to read with file_read_line or file_read_bin.
/// It returns a FileLck object which will be freed when close is called.
FileLck *file_ropen (char *path);

/// Opens a file to write with file_write_line or file_write_bin.
/// It returns a FileLck object which will be freed when close is called.
FileLck *file_wopen (char *path);

/// Opens a file to append with file_write_line or file_write_bin.
/// It returns a FileLck object which will be freed when close is called..
FileLck *file_aopen (char *path);

/// Reads a text file open with file_ropen.
/// It does not delete ends of line.
/// When reading is finished, returns a blank string.
char *file_read_line (FileLck *lck);

/// Writes a text file open with file_wopen or file_aopen.
void file_write_text (FileLck *lck, char *text);

/// Reads a binary file open with file_ropen.
/// When reading is finished, returns an empty Bytes.
Bytes *file_read_bin_buf (FileLck *lck, int buffer);

/// file_read_bin is the same as 'file_read_bin_bf' using a buffer of 8192.
Bytes *file_read_bin (FileLck *lck);

/// Writes a binary file open with file_wopen.
/// Returns 0 if there is no error.
void file_write_bin (FileLck *lck, Bytes *bs);

/// Closes a file open with file_ropen, file_wopen or file_aopen.
void file_close (FileLck *lck);

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Strings constructor.

#ifndef DMC_BUF_H
  #define DMC_BUF_H

///
typedef struct buf_Buf Buf;

/// Initializes a 'buf' with size 150.
Buf *buf_new(void);

/// Initializes a 'buf'.
Buf *buf_bf_new(int buffer_size);

/// Returns the length of the enveloped string.
int buf_len(Buf *this);

/// Returns a reference to the string wrapped. Return is intended to
/// be not modified.
char *buf_str(Buf *this);

/// Adds 'length bytes of 'data' to 'buf'.
/// 'length' must be less or equals to 'strlen(data)'.
/// It is not necessary that 'data' be a null-terminated string, but it must
/// no have characters \0
void buf_add_buf(Buf *this, char *data, int length);

/// Adds 'data' to 'buf'.
void buf_add(Buf *this, char *data);

/// Adds a character.
void buf_cadd(Buf *this, char data);

/// Returns a copy of the enveloped string.
char *buf_to_str(Buf *this);

/// Resets buffer (but does not reduce its size).
void buf_reset(Buf *this);

#endif
// Copyright 22-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Option.

#include "dmc/Js.h"

#ifndef DMC_OPT_H
  #define DMC_OPT_H

///
typedef struct opt_Opt Opt;

/// Creates a full option.
Opt *opt_new (void *value);

/// Creates an empty option.
Opt *opt_empty (void);

/// Returns 1 if option has no value.
int opt_is_empty (Opt *this);

/// Returns 1 if option has value.
int opt_is_full (Opt *this);

/// Throws a illegal_state_exception_t if 'this' is empty.
void *opt_get (Opt *this);

/// Throws a illegal_state_exception_t if 'this' is empty with 'msg' as message.
void *opt_eget (Opt *this, char *msg);

/// Returns value if 'this' is empty.
void *opt_oget (Opt *this, void *value);

/// Returns NULL if 'this' is empty.
void *opt_nget (Opt *this);

///
Js *opt_to_js (Opt *this, Js *(*to)(void *e));

///
Opt *opt_from_js (Js *js, void *(*from)(Js *jse));

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing json strings.

#ifndef DMC_JS_H
  #define DMC_JS_H

#include "dmc/Arr.h"
#include "dmc/Map.h"

/// 'Js' is an alias of 'char'.
typedef struct js_Js Js;

/// Returns '1' if json is "null" or '0' in another case.
int js_is_null (Js *json);

/// Read boolean.
int js_rb (Js *json);

/// Read int.
int js_ri (Js *json);

/// Read long.
long js_rl (Js *json);

/// Read double.
double js_rd (Js *json);

/// Read string.
char *js_rs (Js *json);

/// Read array in an Arr<Js>.
Arr *js_ra (Js *json);

/// Read object in a Map<js>.
Map *js_ro (Js *json);

/// Write a null value.
Js *js_wn(void);

/// Write a boolean value.
Js *js_wb(int value);

/// Write an int value.
Js *js_wi(int n);

/// Write an long value.
Js *js_wl(long n);

/// Write a double value with a maximum of 9 decimal positions.
Js *js_wd(double n);

/// Write a string.
Js *js_ws(char *s);

/// Write an Arr<Js>.
Js *js_wa(Arr *a);

/// Write a Map<Js>.
Js *js_wo(Map *m);

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array of ints.

#ifndef DMC_IARR_H
  #define DMC_IARR_H

#include "Js.h"

///
typedef struct iarr_Iarr Iarr;

///
Iarr *iarr_new(void);

/// buffer must be > 0
Iarr *iarr_bf_new(int buffer);

/// Creates a new array from a C array. For example:
///   Iarr *a = iarr_new_c(3, (int[]){2, 0, 4});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
Iarr *iarr_new_c (int size, int *es);

/// Returns a new Iarr with elements from 0 to 'ix' (exclusive),
Iarr *iarr_left(Iarr *this, int ix);

/// Returns a new Iarr with elements from 'ix' (inclusive) to end of 'this'.
Iarr *iarr_right(Iarr *this, int ix);

/// Returns a new Iarr with elements from 'begin' (inclusive) to
/// to 'end' (exclusive),
Iarr *iarr_sub(Iarr *this, int begin, int end);

///
Iarr *iarr_copy(Iarr *this);

///
int iarr_size(Iarr *this);

///
int iarr_eq(Iarr *this, Iarr *other);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
int iarr_get(Iarr *this, int ix);

///
int *iarr_start(Iarr *this);

///
int *iarr_end(Iarr *this);

///
void iarr_push(Iarr *this, int e);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
void iarr_set(Iarr *this, int ix, int e);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
void iarr_insert(Iarr *this, int ix, int e);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
void iarr_remove(Iarr *this, int ix);

///
void iarr_cat(Iarr *this, Iarr *other);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
void iarr_insert_arr(Iarr *this, int ix, Iarr *other);

/// If begin or end are < 0 then is changed to 'iarr_size - itsValue'.
void iarr_remove_range(Iarr *this, int begin, int end);

/// Removes every element of 'this'. Buffer size is equals to 15.
void iarr_clear (Iarr *this);

/// Removes every element of 'this'.
void iarr_bf_clear (Iarr *this, int buffer);

///
void iarr_reverse(Iarr *this);

///
void iarr_sort(Iarr *this);

///
Js *iarr_to_js(Iarr *this);

///
Iarr *iarr_from_js(Js *js);

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Standard files of dmc.

#ifndef DMC_STD_H
  #define DMC_STD_H

#include <stdio.h>
#include <stdlib.h>
#include <gc.h>

#include "DEFS.h"
#include "Tp.h"
#include "Kv.h"
#include "Tp3.h"
#include "Exc.h"
#include "Opt.h"
#include "Bytes.h"
#include "Arr.h"
#include "It.h"
#include "Map.h"
#include "Buf.h"
#include "str.h"
#include "sys.h"
#include "path.h"
#include "file.h"
#include "Js.h"

#endif
// Copyright 1-Jun-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// List (immutable) structure.

#ifndef DM_LIST_H
  #define DM_LIST_H

///
typedef struct list_List List;
typedef struct opt_Opt Opt;
typedef struct it_It It;
typedef struct arr_Arr Arr;
typedef struct js_Js Js;

/// Creates a new List.
List *list_new(void);

/// Returns the number of elements. (O(n) operation).
int list_count(List *this);

/// Returns every element of 'this' less the first one. If "this" is
/// empty, throws an exception.
List *list_tail(List *this);

/// Returns the first element of 'this'. If "this" is empty.
/// throws an exception.
void *list_head (List *this);

/// Returns the element in 'ix' position -- head is in position 0.
/// (O(n) operation).
Opt *list_get (List *this, int ix);

/// Returns '1' if 'this' is empty.
int list_empty(List *this);

/// Adds 'o' at head. 'o' must be not NULL.
List *list_cons(List *this, void *o);

/// Returns 'this + l'.
List *list_cat(List *this, List *l);

/// list_reverse returns this in reverse order.
List *list_reverse(List *this);

/// list_to_it returns an iterator from top to bottom.
It *list_to_it (List *this);

/// list_from_it return a List with elements of 'it' in reverse order.
List *list_from_it (It *it);

/// list_to_arr returns an Arr with 'this' elements.
Arr *list_to_arr (List *this);

/// list_from_arr returns a list with 'a' elements.
List *list_from_arr (Arr *a);

/// list_to_json returns a serialization of 'this' using 'to' to.
/// convert elements.
Js *list_to_js(List *this, Js *(*to)(void *));

/// list_from_json restores a serialized List using 'from' to convert elements.
List *list_from_js(Js *js, void *(*from)(Js *));

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing globals values.
/// A way to initializes system is:
///   sys_init("app_name");
///   sys_locale("es_ES.utf8");

#ifndef DMC_SYS_H
  #define DMC_SYS_H

#include "dmc/Opt.h"

/// Initializates a normal program and call 'rnd_init()'.
/// After call this function 'sys_home' and 'sys_user' are available.
/// It creates the user directory in "~/.dmCApp/" + 'path'.
void sys_init (char *path);

///
char *sys_home (void);

///
char *sys_uname (void);

///
char *sys_udir (void);

/// Sets LC_ALL, for example:
///   sys_set_locale("es_ES.utf8")
void sys_set_locale (char *language);

/// Returns the current locale.
char *sys_locale (void);

/// Executes 'command', redirecting stderr to stdout, and returns its standard
/// output. If command fails, function returns an empty Opt.
///   return: Opt<char>.
Opt *sys_cmd(char *command);

/// Stops the current thread.
void sys_sleep (int millis);

#endif
// Copyright 01-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Inet server.

#ifndef DMC_ISERVER_H
  #define DMC_ISERVER_H

#include "dmc/std.h"

/// Request read by Iserver.
typedef struct iserver_Rq IserverRq;

/// Returns a error message or an empty string if everithing was well.
char *iserverRq_error (IserverRq *this);

/// Returns Opt<char>. If no request was received, returns 'opt_emtpy'.
/// If 'iserverRq_error' returns an error, throws an ILLEGAL_STATE exception.
Opt *iserverRq_msg (IserverRq *this);

/// Retuns the direction (IPv4) of connected server. Throws an ILLEGAL_STATE
/// exception if no request was read.
/// For being sure about receiving request, 'iserverRq_msg' should be tested.
char *iserverRq_host (IserverRq *this);

/// Writes response in 'this' and close it.
/// Returns an error message or an empty string if everithing was well.
char *iserverRq_write (IserverRq *this, char *response);

///
typedef struct iserver_Iserver Iserver;

///
Iserver *iserver_new (int port);

/// Read text in a not blocking way.
IserverRq *iserver_read (Iserver *this);

///
void iserver_close (Iserver *this);

#endif
// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Decimal number and numeric utilities.

#ifndef DMC_DEC_H
  #define DMC_DEC_H

#include "Js.h"

///
typedef struct dec_Dec Dec;

/// Makes a new Dec. It requires link with -lm.
///   n    : Number which will be rounded to 'scale'.
///   scale: Decimal positions. Maximum scale is 10.
Dec *dec_new(double n, int scale);

/// Returns use '.' as decimal point.
char *dec_to_str(Dec *this);

/// Returns use '.' as thousands separator.
char *dec_int_to_iso(int n);

/// Returns use ',' as thousands separator.
char *dec_int_to_us(int n);

/// Returns use ',' as decimal point and '.' as thousands separator.
char *dec_double_to_iso(double n, int scale);

/// Returns use '.' as decimal point and ',' as thousands separator.
char *dec_double_to_us(double n, int scale);

/// Returns the rounded double value of 'this'.
double dec_n(Dec *this);

/// Returns the scale of 'this'.
int dec_scale(Dec *this);

/// Return has an error gap proportional to digits of 'd1' and 'd2'.
int dec_eq(double d1, double d2);

/// Returns true if d1 == d2 with an error margin of +- gap.
int dec_eq_gap(double d1, double d2, double gap);

/// Returns true if d1 == d2 with an error margin of +- gap.
int dec_eqf_gap(float d1, float d2, float gap);

/// Returns 'true' if all characters of 's' are digits.
/// ("" returns 'true').
int dec_digits(const char *s);

/// Returns a number without thousand separators and
/// with decimal point.
char *dec_regularize_iso(char *s);

/// Returns a number without thousand separators and with
/// decimal point.
char *dec_regularize_us(char *s);

/// Returns 'true' if "s" is a regularized number. If is "" returns 'true'.
/// "xxx.", "." or ".xxx" also return 'true'.
int dec_number(char *s);

///
Js *dec_to_js(Dec *this);

///
Dec *dec_from_js(Js *js);

#endif
// Copyright 23-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Pair of two values.

#ifndef DMC_TP_H
  #define DMC_TP_H

///
typedef struct tp_Tp Tp;

///
Tp *tp_new(void *e1, void *e2);

///
void *tp_e1(Tp *this);

///
void *tp_e2(Tp *this);

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// B64 encoder - decoder

#ifndef DMC_B64_H
  #define DMC_B64_H

#include "dmc/Bytes.h"

///
char *b64_decode(char *b64);

///
Bytes *b64_decode_bytes(char *b64);

///
char *b64_encode(char *s);

///
char *b64_encode_bytes(Bytes *bs);

#endif
