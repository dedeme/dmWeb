// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Model data
class Model {
  public final id: String;
  public final name: String;
  public final doc: String;
  public final paramNames: Array<String>;
  public final paramBases: Array<Float>;
  public final paramBaseIncs: Array<Float>;
  public final paramEnvIncs: Array<Float>;

  function new (
    id: String, name: String, doc: String,
    paramNames: Array<String>, paramBases: Array<Float>,
    paramBaseIncs: Array<Float>, paramEnvIncs: Array<Float>
  ) {
    this.id = id;
    this.name = name;
    this.doc = doc;
    this.paramNames = paramNames;
    this.paramBases = paramBases;
    this.paramBaseIncs = paramBaseIncs;
    this.paramEnvIncs = paramEnvIncs;
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
      a[6].ra().map(e -> e.rf())
    );
  }
}
