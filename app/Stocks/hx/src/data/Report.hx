// Copyright 22-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Opt;

/// General report.
class Report {
  public static final ALL = -1;
  public static final WITH_FEES = -2;

  public var cost(default, null): Float;
  public var profits(default, null): Float;
  public var fees(default, null): Option<Float>;
  public var summary(default, null): Array<ReportSummary>;
  public var anns(default, null): Array<ReportAnn>;

  /// Constructor:
  ///   cost   : Total stocks cost.
  ///   profits: Current profits.
  ///   fees   : Current fees payd. Only has value if report is 'WITH_FEES'.
  ///   summary: Stocks summary.
  ///   anns   : Diary annotations.
  public function new (
    cost: Float, profits: Float, fees: Option<Float>,
    summary: Array<ReportSummary>, anns: Array<ReportAnn>
  ) {
    this.cost = cost;
    this.profits = profits;
    this.fees = fees;
    this.summary = summary;
    this.anns = anns;
  }
}

/// Sumary entry.
class ReportSummary {
  public var nick(default, null): String;
  public var stocks(default, null): Int;
  public var price(default, null): Float;
  public var total(default, null): Float;

  /// Constructor.
  ///   nick  :  Company nick.
  ///   stocks: Stocks.
  ///   price : Price of each stock.
  ///   total : Total value.
  public function new (
    nick: String, stocks: Int, price: Float, total: Float
  ) {
    this.nick = nick;
    this.stocks = stocks;
    this.price = price;
    this.total = total;
  }
}

/// Annotations entry.
class ReportAnn {
  public var date(default, null): Date;
  public var nick(default, null): String;
  public var stocks(default, null): Int;
  public var price(default, null): Float;
  public var total(default, null): Float;
  public var profits(default, null): Option<Float>;
  public var fees(default, null): Option<Float>;

  /// Constructor.
  ///   nick   :  Company nick.
  ///   stocks : Stocks.
  ///   price  : Price of each stock.
  ///   total  : Total value.
  ///   profits: Profits of a sell operation. If operation is a Buy, its value
  ///            is 'None'.
  ///   fees   : Fees of operation. Only has value if report is 'WITH_FEES'.
  public function new (
    date: Date, nick: String, stocks: Int, price: Float, total: Float,
    profits: Option<Float>, fees: Option<Float>
  ) {
    this.date = date;
    this.nick = nick;
    this.stocks = stocks;
    this.price = price;
    this.total = total;
    this.profits = profits;
    this.fees = fees;
  }
}
