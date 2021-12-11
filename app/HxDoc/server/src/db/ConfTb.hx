// Copyright 19-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db;

import dm.File;
import dm.Path;
import dm.Js;
import cm.data.Conf;

/// Configuration table.
class ConfTb {
  static var path: String;

  public static function init (parent: String): Void {
    path = Path.cat([parent, "conf.tb"]);
    if (!File.exists(path)) {
      write(Conf.mkInitial());
    }
  }

  public static function writeJs (js: Js): Void {
    File.write(path, js.to());
  }

  public static function write (cf: Conf): Void {
    writeJs(cf.toJs());
  }

  public static function readJs (): Js {
    return Js.from(File.read(path));
  }

  public static function read (): Conf {
    return Conf.fromJs(readJs());
  }
}
