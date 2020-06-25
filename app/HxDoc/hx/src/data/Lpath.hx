// Copyright 19-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Library paths entry.
class Lpath {
  /// Source name.
  public var lib: String;
  /// Source path.
  public var path: String;
  public var isValid(default, null) = true;
  /// 'true' if Spath must be shown.
  public var isShown = true;

  /// Constructor.
  ///   lib : Source name.
  ///   path: Source path.
  public function new (lib: String, path: String) {
    this.lib = lib;
    this.path = path;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(lib),
      Js.ws(path),
      Js.wb(isValid),
      Js.wb(isShown)
    ]);
  }

  public static function fromJs (js: Js): Lpath {
    var a = js.ra();
    var r = new Lpath(a[0].rs(), a[1].rs());
    r.isValid = a[2].rb();
    r.isShown = a[3].rb();
    return r;
  }
}
