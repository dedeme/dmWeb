// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for GA models

#ifndef DATA_DFLEAS_GA_GABASE_H
  #define DATA_DFLEAS_GA_GABASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct gABase_GABase GABase;

struct gABase_GABase {
  int to_sell;
  double sum;
  double num;
  QmatrixValues *closes;
  int con; // Company number
  int closes_ix; // Index (day) of Qmatrix
  int days_ix;
  double dv;
  double ref;
};

///
GABase *gABase_new(QmatrixValues *closes, int con, int days);

/// Returns Arr[IncrBase]
Arr *gABase_cos (int qnicks, QmatrixValues *closes, int days);

///
Order *gABase_order (
  GABase *this, double q, int days, double strip_to_buy, double strip_to_sell
);

///
double gABase_ref (
  GABase *this, double strip_to_buy, double strip_to_sell
);

#endif
