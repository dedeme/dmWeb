// Copyright 20-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// File path of a table.

package data;

import dm.Js;
import dm.Path;

class Table {
  public final fpath: String;
  public final error: String;

  public function new (fpath: String, error: String) {
    this.fpath = fpath;
    this.error = error;
  }

  public function base (): String {
    return Path.name(fpath);
  }

  public function shortPath (): String {
    return fpath.length > 80
      ? "..." + fpath.substring(fpath.length - 37)
      : fpath
    ;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(fpath),
      Js.ws(error)
    ]);
  }

  public static function fromJs (js: Js): Table {
    final a = js.ra();
    return new Table(
      a[0].rs(),
      a[1].rs()
    );
  }
}
