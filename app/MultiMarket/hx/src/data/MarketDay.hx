// Copyright 19-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

class MarketDay {
  public var date(default, null):String;
  public var hopen(default, null): Int;
  public var mopen(default, null): Int;
  public var hclose(default, null): Int;
  public var mclose(default, null): Int;

  /// Constructor.
  ///   date  : Day.
  ///   hopen : Open hour.
  ///   mopen : Open minute.
  ///   hclose: Close hour.
  ///   mclose: Close minute.
  public function new (date: String, hopen, mopen, hclose, mclose: Int) {
    this.date = date;
    this.hopen = hopen;
    this.mopen = mopen;
    this.hclose = hclose;
    this.mclose = mclose;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(date),
      Js.wi(hopen),
      Js.wi(mopen),
      Js.wi(hclose),
      Js.wi(mclose)
    ]);
  }

  public static function fromJs (js: Js): MarketDay {
    var a = js.ra();
    return new MarketDay(
      a[0].rs(),
      a[1].ri(),
      a[2].ri(),
      a[3].ri(),
      a[4].ri()
    );
  }

}
