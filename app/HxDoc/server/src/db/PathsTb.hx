// Copyright 19-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db;

import dm.File;
import dm.Path;
import dm.Js;
import cm.data.Paths;

/// Paths list table.
class PathsTb {
  static var path: String;

  public static function init (parent: String): Void {
    path = Path.cat([parent, "paths.tb"]);
    if (!File.exists(path)) {
      write(Paths.mkInitial());
    }
  }

  public static function writeJs (js: Js): Void {
    File.write(path, js.to());
  }

  public static function write (paths: Paths): Void {
    writeJs(paths.toJs());
  }

  public static function readJs (): Js {
    return Js.from(File.read(path));
  }

  public static function read (): Paths {
    return Paths.fromJs(readJs());
  }
}
