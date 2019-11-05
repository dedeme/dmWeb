// Copyright 27-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Session data.

#ifndef DATA_SESSION_H
  #define DATA_SESSION_H

#include "dmc/std.h"
#include "dmc/MatchLeague.h"
/*--*/

/// Session data.
///   Arguments:
///     nicks1: Arr-char*
///     l1: MatchLeague
///     nicks2: Arr-char*
///     l2: MatchLeague
///     nicks3: Arr-char*
///     l3: MatchLeague
typedef struct Session_Session Session;

///
Session *session_new (
  Arr *nicks1,
  MatchLeague *l1,
  Arr *nicks2,
  MatchLeague *l2,
  Arr *nicks3,
  MatchLeague *l3
);

/// Arr<char> First division nicks.
Arr *session_nicks1 (Session *this);

/// First division league
MatchLeague *session_l1 (Session *this);

/// Arr<char> Second division nicks.
Arr *session_nicks2 (Session *this);

/// Second division league
MatchLeague *session_l2 (Session *this);

/// Arr<char> Third division nicks.
Arr *session_nicks3 (Session *this);

/// Third division league
MatchLeague *session_l3 (Session *this);

///
Js *session_to_js (Session *this);

///
Session *session_from_js (Js *js);

/*--*/

#endif
