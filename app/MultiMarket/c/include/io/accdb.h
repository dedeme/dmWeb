// Copyright 23-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Accounting data base.

#ifndef IO_ACCDB_H
  #define IO_ACCDB_H

#include "dmc/std.h"
#include "data/Qmatrix.h"
#include "data/Acc.h"
#include "data/Rs.h"

///
void accdb_init (void);

/// Returns a QmatrixValues of quotes, which indices match nicks indices. Values
/// missing are set to '-1'.<br>
/// 'nicks' is Arr[char]
QmatrixValues accdb_dailyq_read (Arr *nicks);

/// if 'nick' is missing, returns -1
double accdb_dailyq_read_nick (char *nick);

/// 'quotes' is Js-> Map[Js->double]
void accdb_dailyq_write (Js *quotes);

/// Returns results including daily quotes and using the current accouning
/// flea model.
RsHistoric *accdb_historic (char *nick);

/// Returns results not including daily quotes and using the current accouning
/// flea model.
RsHistoric *accdb_historic_without_dailyq (char *nick);

/// Returns Arr[AccEntry] with every entry (From before to after)
Arr *accdb_diary_read (void);

/// Returns Js->[Arr[AccEntry]] with last year entries (From after to before)
Js *accdb_diary_read_js (void);

///
void accdb_diary_add (AccEntry *entry);

///
void accdb_diary_remove (char *year, int id);

/// Updates portfolio with current quotes and broker references
void accdb_pf_update (AccPf *pf);

/// Returns [Js-> Arr[char, Darr]].
/// Each row is 'date, Profits [total, acc, risk]'
Js *accdb_profits_read_js (void);

///
void accdb_profits_add (char *date, double total, double acc, double risk);

/// Returns profits with a new record added, but without saving.<p>
/// Returns [Js-> Arr[char, Darr]].
/// Each row is 'date, Profits [total, acc, risk]'
Js *accdb_profits_with (char *date, double total, double acc, double risk);

#endif
