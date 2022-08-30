// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Nick-ponderation data.
class Nick {
  /// Identifier (IBE, TEL...)
  public final id: String;
  /// Ibex ponderation in Bolsa Madrid.
  public final pond: Float;

  function new (
    id: String, pond: Float
  ) {
    this.id = id;
    this.pond = pond;
  }

  public static function fromJs (js: Js): Nick {
    final a = js.ra();
    return new Nick(
      a[0].rs(),
      a[1].rf()
    );
  }
}
