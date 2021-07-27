// Copyright 13-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;
import dm.Dec;

/// Flea record.
class Flea {
  public var param: Float;
  public var name(get, never): String;
  function get_name(): String {
    return Dec.to(param * 1000000, 0);
  }

  function new (param: Float) {
    this.param = param;
  }

  public function toJs () {
    return Js.wa([
      Js.wf(param)
    ]);
  }

  public static function fromJs(js: Js): Flea {
    final a = js.ra();
    return new Flea(
      a[0].rf()
    );
  }
}
