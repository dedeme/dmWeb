// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.acc;

import dm.Js;

// Ledger data
class Ledger {
  public var stocks(default, null): Float;      // + (activo)
  public var cash(default, null): Float;        // + (activo)
  public var capital(default, null): Float;     // - (pasivo)
  public var sells(default, null): Float;       // - (pasivo) sells profits
  public var fees(default, null): Float;        // - (pasivo)
  public var profits(default, null): Float;     // - (pasivo)
  public var differences(default, null): Float; // - (pasivo)

  public function new (
    stocks: Float, cash: Float, capital: Float, sells: Float,
    fees: Float, profits: Float, differences: Float
  ) {
    this.stocks = stocks;
    this.cash = cash;
    this.capital = capital;
    this.sells = sells;
    this.fees = fees;
    this.profits = profits;
    this.differences = differences;
  }

  public static function fromJs (js: Js): Ledger {
    final a = js.ra();
    return new Ledger(
      a[0].rf(),
      a[1].rf(),
      a[2].rf(),
      a[3].rf(),
      a[4].rf(),
      a[5].rf(),
      a[6].rf()
    );
  }
}


