// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Flea model record.
class Fmodel {
  public var id(default, null): String;
  public var name(default, null): String;
  public var parNames(default, null): Array<String>;
  public var parMins(default, null): Array<Float>;
  public var parMaxs(default, null): Array<Float>;
  public var parDecs(default, null): Array<Int>;

  function new (
    id: String, name: String, parNames: Array<String>,
    parMins: Array<Float>, parMaxs: Array<Float>, parDecs: Array<Int>
  ) {
    this.id = id;
    this.name = name;
    this.parNames = parNames;
    this.parMins = parMins;
    this.parMaxs = parMaxs;
    this.parDecs = parDecs;
  }

  public static function fromJs (js: Js): Fmodel {
    final a = js.ra();
    return new Fmodel(
      a[0].rs(),
      a[1].rs(),
      a[2].ra().map(e -> e.rs()),
      a[3].ra().map(e -> e.rf()),
      a[4].ra().map(e -> e.rf()),
      a[5].ra().map(e -> e.ri())
    );
  }
}
