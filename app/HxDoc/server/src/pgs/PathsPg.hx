// Copyright 19-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Cgi;
import dm.Js;
import db.ConfTb;
import db.PathsTb;
import cm.data.Paths;

/// Paths page.
class PathsPg {
  public static function process(mrq: Map<String, Js>): String {
    final rq = Cgi.rqString(mrq, "rq");
    return switch (rq) {
      case "update":
        ConfTb.writeJs(mrq["conf"]);
        final pathsJs = mrq["paths"];
        PathsTb.writeJs(pathsJs);
        final paths = Paths.fromJs(pathsJs);
        for (p in paths.list) {
          p.isValid = dm.File.isDirectory(p.path);
          if (!p.isValid) {
            p.isShown = false;
          }
        }
        Cgi.rp([
          "paths" => paths.toJs()
        ]);
      default: throw new haxe.Exception(
          'Value of rq (${rq}) is not valid'
        );
    }
  }
}
