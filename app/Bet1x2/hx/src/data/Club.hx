// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Club data
class Club {
  /// Indentifier for club and its logo.
  public final id: String;
  /// Complete club name.
  public final name: String;

  public function new (id: String, name: String) {
    this.id = id;
    this.name = name;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(id),
      Js.ws(name)
    ]);
  }

  public static function fromJs (js: Js): Club {
    final a = js.ra();
    return new Club(
      a[0].rs(),
      a[1].rs()
    );
  }
}
