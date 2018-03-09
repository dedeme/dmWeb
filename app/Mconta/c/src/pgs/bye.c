// Copyright 09-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "pgs/bye.h"
#include "backups.h"

CgiRp *pgs_bye(Cgi *cgi, char *session_id) {
  backups_auto(cgi);
  return cgi_del_session(cgi, session_id);
}
