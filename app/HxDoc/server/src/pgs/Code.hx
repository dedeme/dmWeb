// Copyright 20-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;
import dm.File;

/// Code page.
class Code {
  public static function process(mrq: Map<String, Js>): String {
    final path = Cgi.rqString(mrq, "path");
    final r = File.exists(path) ? File.read(path) : "";
    return Cgi.rp(["code" => Js.ws(r)]);
  }
}
