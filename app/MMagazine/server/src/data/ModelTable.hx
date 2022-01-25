// Copyright 03-Jan-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;

class ModelResult {
  public final lastSales: Int;
  public final lastValue: Float;
  public final historicSales: Float;
  public final historicValue: Float;

  function new (
    lastSales: Int, lastValue: Float, historicSales: Float, historicValue:Float
  ) {
    this.lastSales = lastSales;
    this.lastValue = lastValue;
    this.historicSales = historicSales;
    this.historicValue = historicValue;
  }

  public static function fromJs (js: Js): ModelResult {
    final a = js.ra();
    return new ModelResult(
      a[0].ri(),
      a[1].ri() / 1000000,
      a[2].ri() / 1000,
      a[3].ri() / 1000000
    );
  }
}

class ModelTable {
  public final date: String;
  public final days: Int;
  public final results: Map<Int, Array<ModelResult>>;

  function new (
    date: String, days: Int, results: Map<Int, Array<ModelResult>>
  ) {
    this.date = date;
    this.days = days;
    this.results = results;
  }

  public static function fromJs (js: Js): ModelTable {
    final a = js.ra();
    final mrss = new Map<Int, Array<ModelResult>>();
    for (rowJs in a[2].ra()) {
      final fieldsJs = rowJs.ra();
      mrss.set(
        fieldsJs[0].ri(),
        It.from(fieldsJs).drop(1).map(e -> ModelResult.fromJs(e)).to()
      );
    }

    return new ModelTable(
      a[0].rs(),
      a[1].ri(),
      mrss
    );
  }
}
