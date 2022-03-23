// Copyright 07-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Path data.
class Dpath {
  /// Path identifier.
  public final id: String;
  /// Source path.
  public final path: String;
  /// 'true' if the path should be shown.
  public final isShown: Bool;
  /// 'true' if the path is a valid one.
  public final isValid: Bool;

  function new (
    id: String, path: String, isShown: Bool, isValid: Bool
  ) {
    this.id = id;
    this.path = path;
    this.isShown = isShown;
    this.isValid = isValid;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(id),
      Js.ws(path),
      Js.wb(isShown),
      Js.wb(isValid)
    ]);
  }

  public static function fromJs (js: Js): Dpath {
    final a = js.ra();
    return new Dpath(
      a[0].rs(),
      a[1].rs(),
      a[2].rb(),
      a[3].rb()
    );
  }
}
