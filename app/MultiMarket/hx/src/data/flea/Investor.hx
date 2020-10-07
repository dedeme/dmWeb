// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Investor (Manager) data.
class Investor {
  public var model(default, null): Fmodel;
  public var eflea(default, null): Eflea;
  public var name(get, never): String;
  function get_name(): String {
    return model.id + "-" + eflea.flea.name;
  }

  function new (model: Fmodel, eflea: Eflea) {
    this.model = model;
    this.eflea = eflea;
  }

  public static function fromJs (js: Js): Investor {
    final a = js.ra();
    return new Investor(
      Fmodel.fromJs(a[0]),
      Eflea.fromJs(a[1])
    );
  }
}

