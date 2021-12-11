// Copyright 19-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Js;

/// Library paths entry.
class Lpath {
  /// Source name.
  public var lib: String;
  /// Source path.
  public var path: String;
  /// 'true' if path is a directory.
  public var isValid: Bool;
  /// 'true' if Spath must be shown.
  public var isShown: Bool;

  /// Constructor.
  ///   lib    : Source name.
  ///   path   : Source path.
  ///   isValid: 'true' if path is a directory.
  ///   isShown: 'true' if Spath must be shown.
  public function new (
    lib: String, path: String, isValid: Bool, isShown: Bool
  ) {
    this.lib = lib;
    this.path = path;
    this.isValid = isValid;
    this.isShown = isShown;
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
    return new Lpath(
      a[0].rs(),
      a[1].rs(),
      a[2].rb(),
      a[3].rb()
    );
  }
}
