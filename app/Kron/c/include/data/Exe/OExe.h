// Copyright 25-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Opt[Exe*].

#ifndef DATA_EXE_OEXE_H
  #define DATA_EXE_OEXE_H

#include "data/Exe.h"

/// Opt[Exe*].
typedef struct oExe_OExe OExe;

/// Returns a none option.
OExe *oExe_mk_none();

/// Returns an option with a value.
OExe *oExe_mk_some(Exe *value);

/// Returns '1' if 'opt' is none and 0 otherwise.
int oExe_none(OExe *opt);

/// Returns the value of 'opt' or raise a FAIL if it is none.
Exe *oExe_some(OExe *opt);

/// Raise a fail if 'opt' is empty with 'msg' as message.
Exe *oExe_esome (OExe *opt, char *msg);

/// Returns 'value' if 'opt' is empty.
Exe *oExe_osome (OExe *opt, Exe *value);

/// Returns the value of 'opt' or NULL if 'this' is empty.
Exe *oExe_nsome (OExe *opt);

/// Returns this JSONized.
///   this: Container.
char *oExe_to_js (OExe *this);

/// Returns the container from its JSON representation.
///   js  : Container JSONized.
OExe *oExe_from_js (char *js);


//--// Not remove

#endif