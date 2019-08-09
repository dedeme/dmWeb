// Copyright 14-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Flea accounting

#ifndef DATA_FACC_H
  #define DATA_FACC_H

#include "dmc/async.h"
#include "Qmatrix.h"

///
typedef struct Facc_Facc Facc;

/// Returns a new Acc with INTIAL_CAPITAL and without stocks.
///   ncos: Total companies number.
Facc *facc_new(int ncos);

///
double facc_cash(Facc *this);

/// Stocks in portfolio. Each index is a company. Thus acc_pf()[2] returns the
/// number of stocks of company "2".
int *facc_pf(Facc *this);

/// Buys stocks of a company.
///   co: Company
///   price: Price of each stock
///   returns: 1 if operation could be done or 0 otherwise.
int facc_buy(Facc *this, int co, double price);

/// Sells stocks of a company. Every stock will be sold. If the stocks number
/// is '0' it does nothing.
///   co: Company
///   price: Price of each stock
///   returns: 1 if operation could be done (stocks != 0) or 0 otherwise.
int facc_sell(Facc *this, int co, double price);

/// Returns total of assets discounting fees.
///   ncos: Companies number. Its value is equals to the size of 'closes' and
///         'facc_pf()'.
///   closes: Market closes. Each row is a date and in each row each index is
///           a company. Thus closes[0][2] is the close of company "2" in the
///           first date.
///   ix: Index of date (row) to calculate assets
double facc_assets(
  Facc *this,
  int ncos,
  Qmatrix *closes,
  int ix
);

#endif
