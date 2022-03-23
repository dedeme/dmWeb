// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Simulation profits table row.

#ifndef DATA_SIMULATION_SIMPROFITSROW_H
  #define DATA_SIMULATION_SIMPROFITSROW_H

#include "dmc/ADouble.h"
#include "SimProfits.h"

/// Simulation profits table row.
struct simProfitsRow_SimProfitsRow {
  /// Params evaluated.
  ADouble *params;
  /// Weeks weight to calculate historic values.
  int weeks;
  /// Historic values.
  SimProfits *hprofits;
  /// Last values.
  SimProfits *profits;
};

/// Simulation profits table row.
typedef struct simProfitsRow_SimProfitsRow SimProfitsRow;

/// Constructor
SimProfitsRow *simProfitsRow_new (
  ADouble *params, int weeks, SimProfits *hprofits, SimProfits *profits
);

///
char *simProfitsRow_to_js(SimProfitsRow *this);

///
SimProfitsRow *simProfitsRow_from_js(char *js);

#endif
