// Copyright 02-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import data.Prop;
import dm.Js;

class Replacement {
  /// Value of atomic proposition.
  public final key: String;
  /// Replacement.
  public final value: PropT;

  /// Constructor.
  ///   key: Value of atomic proposition.
  ///   value: Replacement.
  public function new (key: String, value: PropT) {
    this.key = key;
    this.value = value;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(key),
      Prop.toJs(value)
    ]);
  }

  public static function fromJs (js: Js): Replacement {
    final a = js.ra();
    return new Replacement(
      a[0].rs(),
      Prop.fromJs(a[1])
    );
  }
}
