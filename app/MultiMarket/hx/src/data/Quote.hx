// Copyright 20-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Quote data.
class Quote {
  public var date(default, null): String;
  public var open(default, null): Float;
  public var close(default, null): Float;
  public var max(default, null): Float;
  public var min(default, null): Float;
  public var vol(default, null): Int;
  public var error(default, null): Bool;

  /// Constructor.
  public function new (
    date: String,
    open: Float,
    close: Float,
    max: Float,
    min: Float,
    vol: Int,
    error: Bool
  ) {
    this.date = date;
    this.open = open;
    this.close = close;
    this.max = max;
    this.min = min;
    this.vol = vol;
    this.error = error;
  }

  public function toString (): String {
    return '${date}:${open}:${close}:' +
           '${max}:${min}:${vol}:${error}';
  }

  // Static  -------------------------------------------------------------------

  public static function fromString (s: String): Quote {
    final q = s.split(":");
    return new Quote(
      q[0],
      Std.parseFloat(q[1]),
      Std.parseFloat(q[2]),
      Std.parseFloat(q[3]),
      Std.parseFloat(q[4]),
      Std.parseInt(q[5]),
      q[6] == "true"
    );
  }

  public static function fromJs (js: Js): Quote {
    final a = js.ra();
    return new Quote(
      a[0].rs(),
      a[1].rf(),
      a[2].rf(),
      a[3].rf(),
      a[4].rf(),
      a[5].ri(),
      a[6].rb()
    );
  }
}
