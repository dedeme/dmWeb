// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Simulation profits table data.

#ifndef DATA_SIMULATION_SIMPROFITSDATA_H
  #define DATA_SIMULATION_SIMPROFITSDATA_H

#include "data/simulation/SimProfitsRow/ASimProfitsRow.h"

/// Simulation profits table data.
struct simProfitsData_SimProfitsData {
  /// Date of data in format YYYYMMDD.
  char *date;
  /// Rows data.
  ASimProfitsRow *rows;
};

/// Simulation profits table data.
typedef struct simProfitsData_SimProfitsData SimProfitsData;

/// Constructor
SimProfitsData *simProfitsData_new (char *date, ASimProfitsRow *rows);

///
char *simProfitsData_to_js(SimProfitsData *this);

///
SimProfitsData *simProfitsData_from_js(char *js);

#endif
