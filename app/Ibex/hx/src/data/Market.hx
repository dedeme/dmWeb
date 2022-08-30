// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// All market data.
class Market {
  /// Models data.
  public final models: Array<Model>;
  /// Nick-ponderations.
  public final nicks: Array<Nick>;
  /// Dates of closes and references.
  public final dates: Array<String>;
  /// Simulations results (one for each model).
  public final results: Array<Rs>;

  function new (
    models: Array<Model>, nicks: Array<Nick>,
    dates: Array<String>, results: Dynamic
  ) {
    this.models = models;
    this.nicks = nicks;
    this.dates = dates;
    this.results = results;
  }

  public static function fromJs (js: Js): Market {
    final o = js.ro();
    return new Market(
      o["models"].ra().map(Model.fromJs),
      o["nicks"].ra().map(Nick.fromJs),
      o["dates"].ra().map(e -> e.rs()),
      o["results"].ra().map(Rs.fromJs)
    );
  }
}
