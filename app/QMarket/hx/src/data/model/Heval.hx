// Copyright 29-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.model;

import dm.Js;

// Historic evaluation data.
class Heval {
  public final sales: Float;
  public final value: Float;

  function new (sales: Float, value: Float) {
    this.sales = sales;
    this.value = value;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wf(sales),
      Js.wf(value)
    ]);
  }

  public static function fromJs (js: Js): Heval {
    final a = js.ra();
    return new Heval(
      a[0].rf(),
      a[1].rf()
    );
  }
}
