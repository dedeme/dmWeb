// Copyright 17-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Reader of nicks and quotes.

#ifndef IO_H
  #define IO_H

#include "dmc/std.h"
#include "dmc/Darr.h"
#include "Data.h"
#include "DEFS.h"

/// Generates data for javascript Store initialization.
DataAll *io_init ();

/// Set language ('en' or 'es')
void io_set_lang (char *lang);

/// Returns language ('en' or 'es')
char *io_lang ();

/// Returns Arr[char] with selected nicks
Arr *io_nicks ();

/// Returns daily quotes (from before to after) of a nick
Darr *io_quotes (enum LeagueGroup group, char *nick);

/// Returns last date of historic quotes
char *io_last_historic_date ();

/// Returns number of daily quotes
char *io_count_daily_quotes ();

/// Returns a league. Nicks is Arr[char].
DataLeague *io_mk_league (Arr *nicks, enum LeagueGroup group);

/// Returns data updated
DataAll *io_update (DataAll *data);

#endif
