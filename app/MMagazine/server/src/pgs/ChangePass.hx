// Copyright 03-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;

/// Change password page.
class ChangePass {
  public static function process(mrq: Map<String, Js>): String {
    final user = Cgi.rqString(mrq, "user");
    final old = Cgi.rqString(mrq, "old");
    final newPass = Cgi.rqString(mrq, "new");

    return Cgi.changePass(user, old, newPass);
  }
}
