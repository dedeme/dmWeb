// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Bets tables.

#ifndef DB_BETS_H
  #define DB_BETS_H

#include "data/Bet/AAOBet.h"

///
void bets_init (char *parent);

///
AAOBet *bets_read (char *year);

///
void bets_write (AAOBet *results);

#endif
