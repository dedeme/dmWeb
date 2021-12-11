// Copyright 19-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db;

import dm.Path;
import dm.File;
import dm.Cgi;

/// Data base.
class Db {
  static var path: String;
  public static function init (): Void {
    final path = Path.cat([Cgi.home, "data"]);
    if (!File.exists(path)) {
      File.mkdir(path);
    }

    ConfTb.init(path);
    PathsTb.init(path);
  }
}
