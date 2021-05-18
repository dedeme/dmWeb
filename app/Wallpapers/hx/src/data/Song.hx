// Copyright 30-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Mac;
import dm.Js;

class Song {
  public final id: String;
  public var level: Int;
  public var sights: Int;
  public var lapse: Float;


  function new (id: String, level: Int, sights: Int, lapse: Float) {
    this.id = id;
    this.level = level;
    this.sights = sights;
    this.lapse = lapse;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(id),
      Js.wi(level),
      Js.wi(sights),
      Js.wf(lapse)
    ]);
  }

  public static function mk (id: String): Song {
    return new Song(id, 1, 0, 0.0);
  }

  public static function fromJs (js: Js): Song {
    final a = js.ra();
    return new Song(
      a[0].rs(),
      a[1].ri(),
      a[2].ri(),
      a[3].rf()
    );
  }
}
