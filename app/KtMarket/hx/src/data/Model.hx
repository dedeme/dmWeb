// Copyright 14-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Model data
class Model {
  // Abbreviated name.
  public final id: String;
  public final name: String;
  public final doc: String;
  public final paramNames: Array<String>;
  public final paramMaxs: Array<Float>;
  public final paramMins: Array<Float>;
  public final paramDecs: Array<Int>;

  function new (
    id: String, name: String, doc: String, paramNames: Array<String>,
    paramMaxs: Array<Float>, paramMins: Array<Float>,
    paramDecs: Array<Int>
  ) {
    this.id = id;
    this.name = name;
    this.doc = doc;
    this.paramNames = paramNames;
    this.paramMaxs = paramMaxs;
    this.paramMins = paramMins;
    this.paramDecs = paramDecs;
  }

  public static function fromJs (js: Js): Model {
    final a = js.ra();
    return new Model(
      a[0].rs(),
      a[1].rs(),
      a[2].rs(),
      a[3].ra().map(e -> e.rs()),
      a[4].ra().map(e -> e.rf()),
      a[5].ra().map(e -> e.rf()),
      a[6].ra().map(e -> e.ri())
    );
  }
}
