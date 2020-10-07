// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;
import dm.Dec;
import data.flea.Fmodel;

/// Manager data.
class Manager {
  /// Model-parameters by default.
  public var base(default, null): ManagerEntry;
  /// Model-parameters por every company.
  public var nicks(default, null): Map<String, ManagerEntry>;

  function new (base: ManagerEntry, nicks: Map<String, ManagerEntry>) {
    this.base = base;
    this.nicks = nicks;
  }

  public static function fromJs (js: Js): Manager {
    final a = js.ra();
    return new Manager(
      ManagerEntry.fromJs(a[0]),
      a[1].rMap(e -> ManagerEntry.fromJs(e))
    );
  }
}

/// Manager entry
class ManagerEntry {
  /// Flea model.
  public var model(default, null): Fmodel;
  /// Flea parameters.
  public var params(default, null): Array<Float>;

  function new (model: Fmodel, params: Array<Float>) {
    this.model = model;
    this.params = params;
  }

  /// Returns true if parameneters of this are equals to other ones.
  public function eqParams(other: ManagerEntry): Bool {
    final tps = params;
    final ops = other.params;
    if (tps.length != ops.length) {
      return false;
    }
    return model.id == model.id &&
      It.range(tps.length).every(ix -> Dec.eq(tps[ix], ops[ix], 0.0000001))
    ;
  }

  public static function fromJs(js: Js): ManagerEntry {
    final a = js.ra();
    return new ManagerEntry(
      Fmodel.fromJs(a[0]),
      a[1].ra().map(e -> e.rf())
    );
  }
}
