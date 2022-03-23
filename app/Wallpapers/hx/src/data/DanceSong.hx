// Copyright 17-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Dance song data.
class DanceSong {
  /// Song level (0(unknown), 1(ok), o 2(well)).
  public final level: Int;
  /// Song speed (0(slow), 1(fast)).
  public final speed: Int;
  /// Song name.
  public final id: String;
  /// Times played in short way.
  public final shortPlays: Int;
  /// Times played in long way.
  public final longPlays: Int;

  public function new (
    level: Int, speed: Int, id: String, shortPlays: Int, longPlays: Int
  ) {
    this.level = level;
    this.speed = speed;
    this.id = id;
    this.shortPlays = shortPlays;
    this.longPlays = longPlays;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(level),
      Js.wi(speed),
      Js.ws(id),
      Js.wi(shortPlays),
      Js.wi(longPlays)
    ]);
  }

  public static function fromJs (js: Js): DanceSong {
    final a = js.ra();
    return new DanceSong(
      a[0].ri(),
      a[1].ri(),
      a[2].rs(),
      a[3].ri(),
      a[4].ri()
    );
  }
}
