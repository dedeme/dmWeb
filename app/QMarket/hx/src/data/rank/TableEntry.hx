// Copyright 08-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.rank;

import data.model.Model;
import dm.Js;

// Rankings table entry.
class TableEntry {
  public final date: String;
  public final modelRank: Array<Model>;

  function new (date: String, modelRank: Array<Model>) {
    this.date = date;
    this.modelRank = modelRank;
  }

  public static function fromJs (js: Js): TableEntry {
    final a = js.ra();
    return new TableEntry(
      a[0].rs(),
      a[1].ra().map(e -> Model.fromJs(e))
    );
  }
}
