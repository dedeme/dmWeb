// Copyright 06-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Main page requests

#ifndef PGS_MAIN_H
  #define PGS_MAIN_H

#include "dmc/all.h"

///
CgiRp *pgs_main(Cgi *cgi, char *session_id, Map/*Json*/ *rq);

#endif
