// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Flea model record.
class Fmodel {
  public var id(default, null): String;
  public var name(default, null): String;
  public var parName(default, null): String;

  function new (id: String, name: String, parName: String) {
    this.id = id;
    this.name = name;
    this.parName = parName;
  }

  public static function fromJs (js: Js): Fmodel {
    final a = js.ra();
    return new Fmodel(
      a[0].rs(),
      a[1].rs(),
      a[2].rs()
    );
  }
}
