// Copyright 30-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Song data.
class Song {
  public var level: Int;
  public var sights: Int;
  public var lapse: Float;
  public final id: String;


  function new (level: Int, sights: Int, lapse: Float, id: String) {
    this.level = level;
    this.sights = sights;
    this.lapse = lapse;
    this.id = id;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(level),
      Js.wi(sights),
      Js.wf(lapse),
      Js.ws(id)
    ]);
  }

  public static function fromJs (js: Js): Song {
    final a = js.ra();
    return new Song(
      a[0].ri(),
      a[1].ri(),
      a[2].rf(),
      a[3].rs()
    );
  }
}
