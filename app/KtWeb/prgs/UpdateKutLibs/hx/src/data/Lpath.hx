// Copyright 09-Feb-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Identifier and path of a library.

package data;

import dm.Js;

class Lpath {
  public final id: String;
  public final fpath: String;
  public final found: Bool;

  public function new (id: String, fpath: String, found: Bool) {
    this.id = id;
    this.fpath = fpath;
    this.found = found;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(id),
      Js.ws(fpath),
      Js.wb(found)
    ]);
  }

  public static function fromJs (js: Js): Lpath {
    final a = js.ra();
    return new Lpath(
      a[0].rs(),
      a[1].rs(),
      a[2].rb()
    );
  }
}
