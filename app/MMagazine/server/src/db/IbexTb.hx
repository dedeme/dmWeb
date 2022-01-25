// Copyright 13-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db;

import dm.Path;
import dm.File;
import dm.Js;
import cm.data.IbexSundays;

/// Table with sunday ibex values.
class IbexTb {
  static var path: String = null;

  public static function init (parent: String): Void {
    path = Path.cat([parent, "ibex.tb"]);
    if (!File.exists(path)) {
      write(new IbexSundays([]));
    }
  }

  public static function write (data: IbexSundays): Void {
    File.write(path, data.toJs().to());
  }

  public static function readJs (): Js {
    return Js.from(File.read(path));
  }

  public static function read (): IbexSundays {
    return IbexSundays.fromJs(readJs());
  }
}
