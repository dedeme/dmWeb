// Copyright 23-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Acc.h"
#include "data/broker.h"
#include "dmc/Dec.h"

/* .
# Data from Sell-Buy accounting entry
-AccSeBu: serial
  nick: char *
  stocks: int
  price: double
===
# Data from Incomes-Withdraws accounting entry
-AccInWi: serial
  ammount: double
===
# Data from Profits-Fees-Differences accounting entry
-AccPrFePdNd: serial
  ammount: double
  cause: char *
===
# Accounting entry
-AccEntry:
  # one of "se", "bu", "in", "wi", "pr", "fe", "pd", "nd" or "cl"
  operation: char *
  # Entry identifier
  @id: int
  date: char *
  -values: void

===
# Historic profits
AccHProfits: SERIAL
  real: double
  acc: double
  risk: double
===
# Historic entry
AccHEntry: SERIAL
  date: char *
  profits: AccHProfits

===
# Acounting ledger
-AccLedger: to
  stocks: double
  cash: double
  capital: double
  sells: double
  fees: double
  profits: double
  differences: double
===
# Portfolio entry
-AccPfEntry: to
  nick: char *
  stocks: int
  price: double
  @quote: double
  @ref: double
===
# Tuple (errors (Arr[char]), ledger, pf)
-AccLedPf
  # Arr[char]
  errors: Arr - char *
  ledger: AccLedger
  pf: AccPf
*/
/*--*/

struct Acc_AccSeBu {
  char *nick;
  int stocks;
  double price;
};

static AccSeBu *_accSeBu_new (char *nick, int stocks, double price) {
  AccSeBu *this = MALLOC(AccSeBu);
  this->nick = nick;
  this->stocks = stocks;
  this->price = price;
  return this;
}

char *accSeBu_nick (AccSeBu *this) {
  return this->nick;
}

int accSeBu_stocks (AccSeBu *this) {
  return this->stocks;
}

double accSeBu_price (AccSeBu *this) {
  return this->price;
}

Js *accSeBu_to_js (AccSeBu *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->nick));
  arr_push(js, js_wi((int)this->stocks));
  arr_push(js, js_wd(this->price));
  return js_wa(js);
}

AccSeBu *accSeBu_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  AccSeBu *this = MALLOC(AccSeBu);
  this->nick = js_rs(*p++);
  this->stocks = js_ri(*p++);
  this->price = js_rd(*p++);
  return this;
}

struct Acc_AccInWi {
  double ammount;
};

static AccInWi *_accInWi_new (double ammount) {
  AccInWi *this = MALLOC(AccInWi);
  this->ammount = ammount;
  return this;
}

double accInWi_ammount (AccInWi *this) {
  return this->ammount;
}

Js *accInWi_to_js (AccInWi *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wd(this->ammount));
  return js_wa(js);
}

AccInWi *accInWi_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  AccInWi *this = MALLOC(AccInWi);
  this->ammount = js_rd(*p++);
  return this;
}

struct Acc_AccPrFePdNd {
  double ammount;
  char *cause;
};

static AccPrFePdNd *_accPrFePdNd_new (double ammount, char *cause) {
  AccPrFePdNd *this = MALLOC(AccPrFePdNd);
  this->ammount = ammount;
  this->cause = cause;
  return this;
}

double accPrFePdNd_ammount (AccPrFePdNd *this) {
  return this->ammount;
}

char *accPrFePdNd_cause (AccPrFePdNd *this) {
  return this->cause;
}

Js *accPrFePdNd_to_js (AccPrFePdNd *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wd(this->ammount));
  arr_push(js, js_ws(this->cause));
  return js_wa(js);
}

AccPrFePdNd *accPrFePdNd_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  AccPrFePdNd *this = MALLOC(AccPrFePdNd);
  this->ammount = js_rd(*p++);
  this->cause = js_rs(*p++);
  return this;
}

struct Acc_AccEntry {
  char *operation;
  int id;
  char *date;
  void *values;
};

static AccEntry *_accEntry_new (
  char *operation,
  int id,
  char *date,
  void *values
) {
  AccEntry *this = MALLOC(AccEntry);
  this->operation = operation;
  this->id = id;
  this->date = date;
  this->values = values;
  return this;
}

char *accEntry_operation (AccEntry *this) {
  return this->operation;
}

int accEntry_id (AccEntry *this) {
  return this->id;
}

void accEntry_set_id (AccEntry *this, int value) {
  this->id = value;
}

char *accEntry_date (AccEntry *this) {
  return this->date;
}

struct Acc_AccHProfits {
  double real;
  double acc;
  double risk;
};

double accHProfits_real (AccHProfits *this) {
  return this->real;
}

double accHProfits_acc (AccHProfits *this) {
  return this->acc;
}

double accHProfits_risk (AccHProfits *this) {
  return this->risk;
}

Js *accHProfits_to_js (AccHProfits *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wd(this->real));
  arr_push(js, js_wd(this->acc));
  arr_push(js, js_wd(this->risk));
  return js_wa(js);
}

AccHProfits *accHProfits_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  AccHProfits *this = MALLOC(AccHProfits);
  this->real = js_rd(*p++);
  this->acc = js_rd(*p++);
  this->risk = js_rd(*p++);
  return this;
}

struct Acc_AccHEntry {
  char *date;
  AccHProfits *profits;
};

char *accHEntry_date (AccHEntry *this) {
  return this->date;
}

AccHProfits *accHEntry_profits (AccHEntry *this) {
  return this->profits;
}

Js *accHEntry_to_js (AccHEntry *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->date));
  arr_push(js, accHProfits_to_js(this->profits));
  return js_wa(js);
}

AccHEntry *accHEntry_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  AccHEntry *this = MALLOC(AccHEntry);
  this->date = js_rs(*p++);
  this->profits = accHProfits_from_js(*p++);
  return this;
}

struct Acc_AccLedger {
  double stocks;
  double cash;
  double capital;
  double sells;
  double fees;
  double profits;
  double differences;
};

static AccLedger *_accLedger_new (
  double stocks,
  double cash,
  double capital,
  double sells,
  double fees,
  double profits,
  double differences
) {
  AccLedger *this = MALLOC(AccLedger);
  this->stocks = stocks;
  this->cash = cash;
  this->capital = capital;
  this->sells = sells;
  this->fees = fees;
  this->profits = profits;
  this->differences = differences;
  return this;
}

double accLedger_stocks (AccLedger *this) {
  return this->stocks;
}

double accLedger_cash (AccLedger *this) {
  return this->cash;
}

double accLedger_capital (AccLedger *this) {
  return this->capital;
}

double accLedger_sells (AccLedger *this) {
  return this->sells;
}

double accLedger_fees (AccLedger *this) {
  return this->fees;
}

double accLedger_profits (AccLedger *this) {
  return this->profits;
}

double accLedger_differences (AccLedger *this) {
  return this->differences;
}

Js *accLedger_to_js (AccLedger *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_wd(this->stocks));
  arr_push(js, js_wd(this->cash));
  arr_push(js, js_wd(this->capital));
  arr_push(js, js_wd(this->sells));
  arr_push(js, js_wd(this->fees));
  arr_push(js, js_wd(this->profits));
  arr_push(js, js_wd(this->differences));
  return js_wa(js);
}

struct Acc_AccPfEntry {
  char *nick;
  int stocks;
  double price;
  double quote;
  double ref;
};

static AccPfEntry *_accPfEntry_new (
  char *nick,
  int stocks,
  double price,
  double quote,
  double ref
) {
  AccPfEntry *this = MALLOC(AccPfEntry);
  this->nick = nick;
  this->stocks = stocks;
  this->price = price;
  this->quote = quote;
  this->ref = ref;
  return this;
}

char *accPfEntry_nick (AccPfEntry *this) {
  return this->nick;
}

int accPfEntry_stocks (AccPfEntry *this) {
  return this->stocks;
}

double accPfEntry_price (AccPfEntry *this) {
  return this->price;
}

double accPfEntry_quote (AccPfEntry *this) {
  return this->quote;
}

void accPfEntry_set_quote (AccPfEntry *this, double value) {
  this->quote = value;
}

double accPfEntry_ref (AccPfEntry *this) {
  return this->ref;
}

void accPfEntry_set_ref (AccPfEntry *this, double value) {
  this->ref = value;
}

Js *accPfEntry_to_js (AccPfEntry *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(this->nick));
  arr_push(js, js_wi((int)this->stocks));
  arr_push(js, js_wd(this->price));
  arr_push(js, js_wd(this->quote));
  arr_push(js, js_wd(this->ref));
  return js_wa(js);
}

struct Acc_AccLedPf {
  Arr *errors;
  AccLedger *ledger;
  AccPf *pf;
};

static AccLedPf *_accLedPf_new (Arr *errors, AccLedger *ledger, AccPf *pf) {
  AccLedPf *this = MALLOC(AccLedPf);
  this->errors = errors;
  this->ledger = ledger;
  this->pf = pf;
  return this;
}

Arr *accLedPf_errors (AccLedPf *this) {
  return this->errors;
}

AccLedger *accLedPf_ledger (AccLedPf *this) {
  return this->ledger;
}

AccPf *accLedPf_pf (AccLedPf *this) {
  return this->pf;
}

/*--*/

/// Constructor for "se", "bu"
AccEntry *accEntry_new1 (
  char *operation,
  int id,
  char *date,
  char *nick,
  int stocks,
  double price
) {
  if (!str_eq(operation, "se") && !str_eq(operation, "bu"))
    EXC_ILLEGAL_ARGUMENT("Bad operation", "se | bu", operation)

  return _accEntry_new(operation, id, date, _accSeBu_new(nick, stocks, price));
}

/// Constructor for "in", "wi"
AccEntry *accEntry_new2 (
  char *operation,
  int id,
  char *date,
  double ammount
) {
  if (!str_eq(operation, "in") && !str_eq(operation, "wi"))
    EXC_ILLEGAL_ARGUMENT("Bad operation", "in | wi", operation)

  return _accEntry_new(operation, id, date, _accInWi_new(ammount));
}

/// Constructor for "pr", "fe", "pd", "nd"
AccEntry *accEntry_new3 (
  char *operation,
  int id,
  char *date,
  double ammount,
  char *cause
) {
  if (
    !str_eq(operation, "pr") && !str_eq(operation, "fe") &&
    !str_eq(operation, "pd") && !str_eq(operation, "nd")
  )
    EXC_ILLEGAL_ARGUMENT("Bad operation", "pr | fe | pd | nd", operation)

  return _accEntry_new(operation, id, date, _accPrFePdNd_new(ammount, cause));
}

/// Constructor for "cl"
AccEntry *accEntry_new4 (
  char *operation,
  int id,
  char *date
) {
  if (!str_eq(operation, "cl"))
    EXC_ILLEGAL_ARGUMENT("Bad operation", "cl", operation)

  return _accEntry_new(operation, id, date, NULL);
}

Js *accEntry_to_js(AccEntry *this) {
  char *operation = this->operation;

  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, js_ws(operation));
  arr_push(js, js_wi((int)this->id));
  arr_push(js, js_ws(this->date));

  // Arr[Js]
  Arr *a = arr_new();
  char key = *operation;
  if (key == 'b' || key == 's') {
    a = js_ra(accSeBu_to_js((AccSeBu *)this->values));
  } else if (key == 'f' || key == 'n' || key == 'p') {
    a = js_ra(accPrFePdNd_to_js((AccPrFePdNd *)this->values));
  } else if (key == 'i' || key == 'w') {
    a = js_ra(accInWi_to_js((AccInWi *)this->values));
  }
  EACH(a, Js, j)
    arr_push(js, j);
  _EACH

  return js_wa(js);
}

AccEntry *accEntry_from_js(Js *js) {
  // Arr[Js]
  Arr *ajs = js_ra(js);
  Js **p = (Js **)arr_start(ajs);
  AccEntry *this = MALLOC(AccEntry);
  this->operation = js_rs(*p++);
  this->id = js_ri(*p++);
  this->date = js_rs(*p++);
  char key = *(this->operation);
  // Arr[Js]
  Arr *a = arr_new();
  if (key == 'b' || key == 's') {
    arr_push(a, *p++);
    arr_push(a, *p++);
    arr_push(a, *p++);
    this->values = (void *)accSeBu_from_js(js_wa(a));
  } else if (key == 'f' || key == 'n' || key == 'p') {
    arr_push(a, *p++);
    arr_push(a, *p++);
    this->values = (void *)accPrFePdNd_from_js(js_wa(a));
  } else if (key == 'i' || key == 'w') {
    arr_push(a, *p++);
    this->values = (void *)accInWi_from_js(js_wa(a));
  } else {
    this->values = NULL;
  }
  return this;
}

static void pf_add(AccPf *pf, char *nick, int stocks, double price) {
  EACH(pf, AccPfEntry, e)
    if (str_eq(e->nick, nick)) {
      int sum = e->stocks + stocks;
      double value = (e->stocks * e->price + stocks * price) / sum;
      e->stocks = sum;
      e->price = value;
      return;
    }
  _EACH
  arr_push(pf, _accPfEntry_new(nick, stocks, price, -1, -1));
}

// 'annotations' is Arr[AccEntry]
AccLedPf *accLedPf_new(Arr *annotations) {
  // Arr[char]
  Arr *errors = arr_new();

  Dec *stocks = dec_new(0, 2); // +
  Dec *cash = dec_new(0, 2); // +
  Dec *capital = dec_new(0, 2); // -
  Dec *sells = dec_new(0, 2); // -
  Dec *fees = dec_new(0, 2); // -
  Dec *profits = dec_new(0, 2); // -
  Dec *differences = dec_new(0, 2); // -

  AccPf *pf = arr_new();

  EACH_IX(annotations, AccEntry, e, ix)
    char *operation = e->operation;
    char key = *operation;
    if (key == 'b') {
      AccSeBu *vs = e->values;
      if (vs->stocks <= 0) {
        arr_push(errors, str_f(
          "%s: Buying %d (<= 0) stocks", (char *)accEntry_to_js(e), vs->stocks
        ));
        continue;
      }
      if (vs->price < 0) {
        arr_push(errors, str_f(
          "%s: Buying price %.4f (< 0)", (char *)accEntry_to_js(e), vs->price
        ));
        continue;
      }

      pf_add(pf, vs->nick, vs->stocks, vs->price);
      Dec *ammount = dec_new(vs->stocks * vs->price, 2);
      Dec *fs = dec_n(ammount) > 0
        ? dec_new(broker_fees(dec_n(ammount)), 2)
        : dec_new(0, 2)
      ;
      stocks = dec_new(dec_n(stocks) + dec_n(ammount), 2);
      fees = dec_new(dec_n(fees) + dec_n(fs), 2);
      cash = dec_new(dec_n(cash) - dec_n(ammount) - dec_n(fs), 2);
      continue;
    }
    if (key == 's') {
      AccSeBu *vs = e->values;
      if (vs->stocks <= 0) {
        arr_push(errors, str_f(
          "%s: Selling %d (<= 0) stocks", (char *)accEntry_to_js(e), vs->stocks
        ));
        continue;
      }
      if (vs->price <= 0) {
        arr_push(errors, str_f(
          "%s: Selling price %.4f (<= 0)", (char *)accEntry_to_js(e), vs->price
        ));
        continue;
      }
      char *nick = vs->nick;

      int missing = 1;
      int del = -1;
      int stocks_sold = vs->stocks;
      Dec *cost = dec_new(0, 2);
      EACH_IX(pf, AccPfEntry, pfe, ix)
        if (str_eq(pfe->nick, nick)) {
          missing = 0;
          if (pfe->stocks == stocks_sold) {
            cost = dec_new(stocks_sold * pfe->price, 2);
            del = ix;
          } else if (pfe->stocks > stocks_sold) {
            cost = dec_new(stocks_sold * pfe->price, 2);
            pfe->stocks -= stocks_sold;
          } else {
            arr_push(errors, str_f(
              "%s: Selling %d stocks when in cartera there are only %d",
              (char *)accEntry_to_js(e), stocks_sold, pfe->stocks
            ));
            stocks_sold = pfe->stocks;
            cost = dec_new(stocks_sold * pfe->price, 2);
            del = ix;
          }
          break;
        }
      _EACH

      if (missing) {
        arr_push(errors, str_f(
          "%s: Nick is missing in portfolio", (char *)accEntry_to_js(e)
        ));
      } else {
        if (del != -1) arr_remove(pf, del);

        Dec *ammount = dec_new(stocks_sold * vs->price, 2);
        Dec *fs = dec_new(broker_fees(dec_n(ammount)), 2);
        stocks = dec_new(dec_n(stocks) - dec_n(cost), 2);
        sells = dec_new(dec_n(sells) - dec_n(ammount) + dec_n(cost), 2);
        fees = dec_new(dec_n(fees) + dec_n(fs), 2);
        cash = dec_new(dec_n(cash) + dec_n(ammount) - dec_n(fs), 2);
      }
      continue;
    }
    if (key == 'f') {
      AccPrFePdNd *vs = e->values;
      double ammount = accPrFePdNd_ammount(vs);
      cash = dec_new(dec_n(cash) - ammount, 2);
      fees = dec_new(dec_n(fees) + ammount, 2);
      continue;
    }
    if (key == 'n') {
      AccPrFePdNd *vs = e->values;
      double ammount = accPrFePdNd_ammount(vs);
      cash = dec_new(dec_n(cash) - ammount, 2);
      differences = dec_new(dec_n(differences) + ammount, 2);
      continue;
    }
    if (key == 'p' && operation[1] == 'd') {
      AccPrFePdNd *vs = e->values;
      double ammount = accPrFePdNd_ammount(vs);
      cash = dec_new(dec_n(cash) + ammount, 2);
      differences = dec_new(dec_n(differences) - ammount, 2);
      continue;
    }
    if (key == 'p' && operation[1] == 'r') {
      AccPrFePdNd *vs = e->values;
      double ammount = accPrFePdNd_ammount(vs);
      cash = dec_new(dec_n(cash) + ammount, 2);
      profits = dec_new(dec_n(profits) - ammount, 2);
      continue;
    }
    if (key == 'i') {
      AccInWi *vs = e->values;
      double ammount = accInWi_ammount(vs);
      cash = dec_new(dec_n(cash) + ammount, 2);
      capital = dec_new(dec_n(capital) - ammount, 2);
      continue;
    }
    if (key == 'w') {
      AccInWi *vs = e->values;
      double ammount = accInWi_ammount(vs);
      cash = dec_new(dec_n(cash) - ammount, 2);
      capital = dec_new(dec_n(capital) + ammount, 2);
      continue;
    }
    capital = dec_new(
      dec_n(capital) + dec_n(sells) + dec_n(fees) +
      dec_n(profits) + dec_n(differences),
      2
    );
    sells = dec_new(0, 2);
    fees = dec_new(0, 2);
    profits = dec_new(0, 2);
    differences = dec_new(0, 2);
  _EACH

  return _accLedPf_new(
    errors,
    _accLedger_new(
      dec_n(stocks), dec_n(cash), dec_n(capital), dec_n(sells),
      dec_n(fees), dec_n(profits), dec_n(differences)
    ),
    pf
  );
}

Darr *accLedPf_profits (AccLedPf *data) {
  AccLedger *l = accLedPf_ledger(data);
  double acc = -l->sells - l->fees - l->profits - l->differences;
  double total = acc;
  double risk = acc;
  EACH(accLedPf_pf(data), AccPfEntry, e)
    total += e->stocks * (e->quote - e->price);
    risk += e->stocks * (e->ref - e->price);
  _EACH
  Darr *r = darr_new();
  darr_push(r, total);
  darr_push(r, acc);
  darr_push(r, risk);
  return r;
}
