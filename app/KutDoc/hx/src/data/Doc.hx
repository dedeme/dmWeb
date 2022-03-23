// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Module documentation data.
class Doc {
  /// Module documentation.
  public final doc: String;
  /// Functions documentation.
  public final functions: Array<DocEntry>;
  /// Values documentation.
  public final values: Array<DocEntry>;

  function new (
    doc: String, functions: Array<DocEntry>, values: Array<DocEntry>
  ) {
    this.doc = doc;
    this.functions = functions;
    this.values = values;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(doc),
      Js.wa(functions.map(e -> e.toJs())),
      Js.wa(values.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): Doc {
    final a = js.ra();
    return new Doc(
      a[0].rs(),
      a[1].ra().map(DocEntry.fromJs),
      a[2].ra().map(DocEntry.fromJs)
    );
  }
}
