// Copyright 17-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Daily chart data
class DailyChart {
  public var nick(default, null): String;
  public var close(default, null): Float;
  public var hours(default, null): Array<Int>;
  public var quotes(default, null): Array<Float>;
  public var managersData(default, null): Array<DailyChartData>;

  function new (
    nick: String,
    close: Float,
    hours: Array<Int>,
    quotes: Array<Float>,
    managersData: Array<DailyChartData>
  ) {
    this.nick = nick;
    this.close = close;
    this.hours = hours;
    this.quotes = quotes;
    this.managersData = managersData;
  }

  public static function fromJs(js: Js): DailyChart {
    final a = js.ra();
    return new DailyChart(
      a[0].rs(),
      a[1].rf(),
      a[2].ra().map(e -> e.ri()),
      a[3].ra().map(e -> e.rf()),
      a[4].ra().map(e -> DailyChartData.fromJs(e))
    );
  }
}

/// Manager data of DailyChart.
class DailyChartData {
    public var stocks(default, null): Int;
    public var price(default, null): Float;
    public var ref(default, null): Float;

  function new (stocks: Int, price: Float, ref: Float) {
    this.stocks = stocks;
    this.price = price;
    this.ref = ref;
  }

  public static function fromJs(js: Js): DailyChartData {
    final a = js.ra();
    return new DailyChartData(
      a[0].ri(),
      a[1].rf(),
      a[2].rf()
    );
  }
}
