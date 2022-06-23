// Copyright 18-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Nick data.
class Nick {
  public final id: Int;
  public final name: String;
  public final isSel: Bool;

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

// NickName - String container.
class NickNameStr {
  public final nick: String;
  public final value: String;

  public function new (nkName: String, value: String) {
    this.nick = nkName;
    this.value = value;
  }

  public static function fromJs (js: Js): NickNameStr {
    final a = js.ra();
    return new NickNameStr(
      a[0].rs(),
      a[1].rs()
    );
  }
}
