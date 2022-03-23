// Copyright 28-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Simulation profits structure.

#ifndef DATA_SIMULATION_SIMPROFITS_H
  #define DATA_SIMULATION_SIMPROFITS_H

/// Simulation profits structure.
struct simProfits_SimProfits {
  /// Total profits.
  double total;
  /// Cash profits.
  double cash;
  /// Refence (risk) profits.
  double ref;
};

/// Simulation profits data.
typedef struct simProfits_SimProfits SimProfits;

/// Constructor
SimProfits *simProfits_new (double total, double cash, double ref);

/// Returns a new SimProfits with 'values of s1' + 'values of s2'.
SimProfits *simProfits_sum (SimProfits *s1, SimProfits *s2);

/// Returns a new SimProfits with 'values of this' / 'n'.
SimProfits *simProfits_div (SimProfits *this, double n);

///
char *simProfits_to_js(SimProfits *this);

///
SimProfits *simProfits_from_js(char *js);

#endif
