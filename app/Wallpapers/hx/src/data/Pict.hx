// Copyright 30-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

// Picture data.
class Pict {
  /// Picture level (1, 2, o 3).
  public final level: Int;
  /// Number of sights in normal mode.
  public final sights: Int;
  /// Picture name.
  public final id: String;

  public function new (level: Int, sights: Int, id: String) {
    this.level = level;
    this.sights = sights;
    this.id = id;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(level),
      Js.wi(sights),
      Js.ws(id)
    ]);
  }

  public static function fromJs (js: Js): Pict {
    final a = js.ra();
    return new Pict(
      a[0].ri(),
      a[1].ri(),
      a[2].rs()
    );
  }
}
