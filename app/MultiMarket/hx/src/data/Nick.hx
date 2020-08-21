// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Nick data.
class Nick {
  public var id(default, null): Int;
  public var name(default, null): String;
  public var isSel(default, null): Bool;

  /// Constructor.
  ///   id   : Nick identifier.
  ///   name : Nick name (TEF, SGRE, etc.)
  ///   isSel: If nick is selected to operate.
  public function new (id: Int, name: String, isSel: Bool) {
    this.id = id;
    this.name = name;
    this.isSel = isSel;
  }

  public static function fromJs (js: Js): Nick {
    final a = js.ra();
    return new Nick(
      a[0].ri(),
      a[1].rs(),
      a[2].rb()
    );
  }
}
