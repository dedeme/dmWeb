// Copyright 23-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Accounting types.

#ifndef DATA_ACC_H
  #define DATA_ACC_H

#include "dmc/async.h"
#include "dmc/Darr.h"

/// Arr[AccPdEntry]
typedef Arr AccPf;

/*--*/

/// Data from Sell-Buy accounting entry
///   Arguments:
///     nick: char*
///     stocks: int
///     price: double
typedef struct Acc_AccSeBu AccSeBu;

///
char *accSeBu_nick (AccSeBu *this);

///
int accSeBu_stocks (AccSeBu *this);

///
double accSeBu_price (AccSeBu *this);

///
Js *accSeBu_to_js (AccSeBu *this);

///
AccSeBu *accSeBu_from_js (Js *js);

/// Data from Incomes-Withdraws accounting entry
///   Arguments:
///     ammount: double
typedef struct Acc_AccInWi AccInWi;

///
double accInWi_ammount (AccInWi *this);

///
Js *accInWi_to_js (AccInWi *this);

///
AccInWi *accInWi_from_js (Js *js);

/// Data from Profits-Fees-Differences accounting entry
///   Arguments:
///     ammount: double
///     cause: char*
typedef struct Acc_AccPrFePdNd AccPrFePdNd;

///
double accPrFePdNd_ammount (AccPrFePdNd *this);

///
char *accPrFePdNd_cause (AccPrFePdNd *this);

///
Js *accPrFePdNd_to_js (AccPrFePdNd *this);

///
AccPrFePdNd *accPrFePdNd_from_js (Js *js);

/// Accounting entry
///   Arguments:
///     operation: char*
///     id: int
///     date: char*
///     values: void
typedef struct Acc_AccEntry AccEntry;

/// one of "se", "bu", "in", "wi", "pr", "fe", "pd", "nd" or "cl"
char *accEntry_operation (AccEntry *this);

/// Entry identifier
int accEntry_id (AccEntry *this);

///
void accEntry_set_id (AccEntry *this, int value);

///
char *accEntry_date (AccEntry *this);

/// Historic profits
///   Arguments:
///     real: double
///     acc: double
///     risk: double
typedef struct Acc_AccHProfits AccHProfits;

///
double accHProfits_real (AccHProfits *this);

///
double accHProfits_acc (AccHProfits *this);

///
double accHProfits_risk (AccHProfits *this);

///
Js *accHProfits_to_js (AccHProfits *this);

///
AccHProfits *accHProfits_from_js (Js *js);

/// Historic entry
///   Arguments:
///     date: char*
///     profits: AccHProfits
typedef struct Acc_AccHEntry AccHEntry;

///
char *accHEntry_date (AccHEntry *this);

///
AccHProfits *accHEntry_profits (AccHEntry *this);

///
Js *accHEntry_to_js (AccHEntry *this);

///
AccHEntry *accHEntry_from_js (Js *js);

/// Acounting ledger
///   Arguments:
///     stocks: double
///     cash: double
///     capital: double
///     sells: double
///     fees: double
///     profits: double
///     differences: double
typedef struct Acc_AccLedger AccLedger;

///
double accLedger_stocks (AccLedger *this);

///
double accLedger_cash (AccLedger *this);

///
double accLedger_capital (AccLedger *this);

///
double accLedger_sells (AccLedger *this);

///
double accLedger_fees (AccLedger *this);

///
double accLedger_profits (AccLedger *this);

///
double accLedger_differences (AccLedger *this);

///
Js *accLedger_to_js (AccLedger *this);

/// Portfolio entry
///   Arguments:
///     nick: char*
///     stocks: int
///     price: double
///     quote: double
///     ref: double
typedef struct Acc_AccPfEntry AccPfEntry;

///
char *accPfEntry_nick (AccPfEntry *this);

///
int accPfEntry_stocks (AccPfEntry *this);

///
double accPfEntry_price (AccPfEntry *this);

///
double accPfEntry_quote (AccPfEntry *this);

///
void accPfEntry_set_quote (AccPfEntry *this, double value);

///
double accPfEntry_ref (AccPfEntry *this);

///
void accPfEntry_set_ref (AccPfEntry *this, double value);

///
Js *accPfEntry_to_js (AccPfEntry *this);

/// Tuple (errors (Arr[char]), ledger, pf)
///   Arguments:
///     errors: Arr-char*
///     ledger: AccLedger
///     pf: AccPf
typedef struct Acc_AccLedPf AccLedPf;

/// Arr[char]
Arr *accLedPf_errors (AccLedPf *this);

///
AccLedger *accLedPf_ledger (AccLedPf *this);

///
AccPf *accLedPf_pf (AccLedPf *this);

/*--*/

/// Constructor for "se", "bu"
AccEntry *accEntry_new1 (
  char *operation,
  int id,
  char *date,
  char *nick,
  int stocks,
  double price
);

/// Constructor for "in", "wi"
AccEntry *accEntry_new2 (
  char *operation,
  int id,
  char *date,
  double ammount
);

/// Constructor for "pr", "fe", "pd", "nd"
AccEntry *accEntry_new3 (
  char *operation,
  int id,
  char *date,
  double ammount,
  char *cause
);

/// Constructor for "cl"
AccEntry *accEntry_new4 (
  char *operation,
  int id,
  char *date
);

///
Js *accEntry_to_js(AccEntry *this);

///
AccEntry *accEntry_from_js(Js *js);

/// 'annotations' is Arr[AccEntry]
AccLedPf *accLedPf_new(Arr *annotations);

/// Returns profits [total, accounting, risk]
Darr *accLedPf_profits (AccLedPf *data);

#endif
