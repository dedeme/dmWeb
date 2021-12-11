// Copyright 14-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.model;

import dm.Js;

/// Model ranking data.
class Rank {
  public var date(default, null): String;
  public var models(default, null): Array<Model>;

  function new (date: String, models: Array<Model>) {
    this.date = date;
    this.models = models;
  }

  public function toJs(): Js {
    return Js.wa([
      Js.ws(date),
      Js.wa(models.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs(js: Js): Rank {
    final a = js.ra();
    return new Rank(
      a[0].rs(),
      a[1].ra().map(e -> Model.fromJs(e))
    );
  }
}
