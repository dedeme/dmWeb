// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Model data.
class Model {
  /// Identifier.
  public final id: String;
  /// Complete name.
  public final name: String;
  /// Documentation.
  public final doc: String;
  /// Parameters.
  public final params: Array<Float>;

  function new (
    id: String, name: String, doc: String, params: Array<Float>
  ) {
    this.id = id;
    this.name = name;
    this.doc = doc;
    this.params = params;
  }

  public static function fromJs (js: Js): Model {
    final a = js.ra();
    return new Model(
      a[0].rs(),
      a[1].rs(),
      a[2].rs(),
      a[3].ra().map(e -> e.rf())
    );
  }
}
