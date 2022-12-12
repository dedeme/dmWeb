// Copyright 08-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Flea model
class FModel {
  public final modelId: String;
  public final params: Array<Float>;

  function new (modelId: String, params: Array<Float>) {
    this.modelId = modelId;
    this.params = params;
  }

  public static function fromJs (js: Js): FModel {
    final a = js.ra();
    return new FModel(
      a[0].rs(),
      a[1].ra().map(j -> j.rf())
    );
  }
}
