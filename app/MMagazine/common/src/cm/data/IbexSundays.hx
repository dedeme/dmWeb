// Copyright 13-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package cm.data;

import dm.Tp;
import dm.Js;
import dm.It;
import dm.Dt;
import dm.Opt;
import dm.Dec;

/// Sunday data of ibex.
/// IbexSundays can have no data.
class IbexSundays {
  final data: Array<Tp<Date, Float>>;

  public function new (data: Array<Tp<Date, Float>>) {
    this.data = data;
  }

  public function dates (): Array<Date> {
    return data.map(tp -> tp.e1);
  }

  public function values (): Array<Float> {
    return data.map(tp -> tp.e2);
  }

  public function ratios (): Array<Float> {
    var base = 0.0;
    if (data.length > 0) {
      base = data[0].e2;
    }

    return values().map(v -> (v - base) / base);
  }

  /// Adds a new entry if there is no another with the same date.
  public function add (date: Date, value: Float): Void {
    final day = Dt.to(date);
    if (It.from(data).indexf(tp -> Dt.to(tp.e1) == day) == -1)
      data.push(new Tp(date, value));
  }

  public function toJs (): Js {
    return Js.wa(data.map(tp -> Js.wa([
      Js.ws(Dt.to(tp.e1)),
      Js.ws(Dec.to(tp.e2, 2))
    ])));
  }

  public static function fromJs (js: Js): IbexSundays {
    return new IbexSundays(
      js.ra().map(ejs -> {
        final a = ejs.ra();
        return new Tp(
          Opt.eget(Dt.from(a[0].rs())),
          Opt.eget(Dec.from(a[1].rs()))
        );
      })
    );
  }

}
