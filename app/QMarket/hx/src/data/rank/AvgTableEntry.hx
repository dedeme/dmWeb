// Copyright 08-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.rank;

import data.model.Model;
import dm.Js;
import dm.It;

// Average rankings table entry.
class AvgTableEntry {
  public final date: String;
  public final rowRank: Array<Array<Model>>;

  function new (date: String, rowRank: Array<Array<Model>>) {
    this.date = date;
    this.rowRank = rowRank;
  }

  public static function evalRow (row: Array<Model>): Float {
    return It.from(row).reduce(0.0, (s, e) -> s + e.hevaluation.value) /
      row.length;
  }

  public static function salesRow (row: Array<Model>): Float {
    return It.from(row).reduce(0.0, (s, e) -> s + e.hevaluation.sales) /
      row.length;
  }

  public static function fromJs (js: Js): AvgTableEntry {
    final a = js.ra();
    return new AvgTableEntry(
      a[0].rs(),
      a[1].ra().map(rowJs -> rowJs.ra().map(mdJs -> Model.fromJs(mdJs)))
    );
  }
}
