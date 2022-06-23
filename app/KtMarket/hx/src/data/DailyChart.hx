// Copyright 20-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Daily chart data
class DailyChart {
  /// Company nick.
  public final nick: String;
  /// Previous market day close. For each investor data (d), if d.ref > close,
  /// its position is to buy.
  public final close: Float;
  /// Hours of each daily quote.
  public final hours: Array<Int>;
  /// Daily quotes.
  public final quotes: Array<Float>;
  /// Dates of each investor (0...).
  public final investorsData: Array<DailyChartData>;

  function new (
    nick: String,
    close: Float,
    hours: Array<Int>,
    quotes: Array<Float>,
    investorsData: Array<DailyChartData>
  ) {
    this.nick = nick;
    this.close = close;
    this.hours = hours;
    this.quotes = quotes;
    this.investorsData = investorsData;
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

/// Investor data for DailyChart.
class DailyChartData {
  /// Investor model identifier.
  public final modelId: String;
  /// Model parameters.
  public final params: Array<Float>;
  /// Stocks number in portfolio. 0 if the company is not en portfolio.
  public final stocks: Int;
  /// Stock value.
  public final price: Float;
  /// Reference of buy-sell
  public final ref: Float;

  function new (
    modelId: String, params: Array<Float>, stocks: Int, price: Float, ref: Float
  ) {
    this.modelId = modelId;
    this.params = params;
    this.stocks = stocks;
    this.price = price;
    this.ref = ref;
  }

  public static function fromJs(js: Js): DailyChartData {
    final a = js.ra();
    return new DailyChartData(
      a[0].rs(),
      a[1].ra().map(e -> e.rf()),
      a[2].ri(),
      a[3].rf(),
      a[4].rf()
    );
  }
}
