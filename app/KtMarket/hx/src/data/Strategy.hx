// Copyright 15-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Dec;
import dm.Js;

/// Strategy data
class Strategy {
  public var model(default, null): Model;
  /// Flea parameters.
  public var params(default, null): Array<Float>;

  function new (model: Model, params: Array<Float>) {
    this.model = model;
    this.params = params;
  }

  /// Returns true if model and parameters of this are equals to 'other' ones.
  public function eqParams(other: Strategy): Bool {
    if (model.id != other.model.id) return false;
    for (i in 0...params.length) {
      if (!Dec.eq(params[i], other.params[i], 0.0000001)) return false;
    }
    return true;
  }

  public static function fromJs(js: Js): Strategy {
    final a = js.ra();
    return new Strategy(
      Model.fromJs(a[0]),
      a[1].ra().map(e -> e.rf())
    );
  }
}

