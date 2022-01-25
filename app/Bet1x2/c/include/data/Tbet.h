// Copyright 25-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Teams and bet conditions. Used to read sportium pages.

#ifndef DATA_TBET_H
  #define DATA_TBET_H

#include "data/Bet.h"

/// Teams and bet conditions. Used to read sportium pages.
struct tbet_Tbet {
  int day;
  char *home;
  char *out;
  Bet *bet;
};

/// Teams and bet conditions. Used to read sportium pages.
typedef struct tbet_Tbet Tbet;

///
Tbet *tbet_new(int day, char *home, char *out, Bet *bet);

///
char *tbet_to_js (Tbet *this);

///
Tbet *tbet_from_js(char *js);

#endif
