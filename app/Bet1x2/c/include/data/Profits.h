// Copyright 29-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Profits data.

#ifndef DATA_PROFITS_H
  #define DATA_PROFITS_H

/// Profits data.
struct profits_Profits {
  char *description;
  int hits;
  int fails;
  double amount;
};

/// Profits data.
typedef struct profits_Profits Profits;

///
Profits *profits_new (char *description, int hits, int fails, double amount);

///
char *profits_to_js (Profits *this);

///
Profits *profits_from_js(char *js);

#endif
