// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Model results data.
class Rs {
  /// Identifier (CM, APPX...)
  public final model: String;
  /// Results.
  public final results: Array<NkRs>;

  function new (model: String, results: Array<NkRs>) {
    this.model = model;
    this.results = results;
  }

  public static function fromJs (js: Js): Rs {
    final a = js.ra();
    return new Rs(
      a[0].rs(),
      a[1].ra().map(NkRs.fromJs)
    );
  }
}
