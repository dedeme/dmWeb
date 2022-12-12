// Copyright 08-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Investor (flea) data
class Flea {
  public final id: Int;
  public final cycle: Int;
  public final isMale: Bool;
  // Ordered
  public final models: Array<FModel>;
  public final assets: Float;

  function new (
    id: Int, cycle: Int, isMale: Bool, models: Array<FModel>, assets: Float
  ) {
    models.sort((m1, m2) -> dm.Str.compare(m1.modelId, m2.modelId));
    this.id = id;
    this.cycle = cycle;
    this.isMale = isMale;
    this.models = models;
    this.assets = assets;
  }

  public function fmtId (): String {
    var r = "" + id;
    while (r.length < 9) r = "0" + r;
    return r;
  }

  public function fmtCycle (): String {
    var r = "" + cycle;
    while (r.length < 4) r = "0" + r;
    return r;
  }

  public function fmtModels (): Array<String> {
    function fmt (md: FModel): String {
      var id = md.modelId;
      while (id.length < 5) id = id + " ";
      return id + "[" +
        md.params.map(n -> Fns.nFormat2(n, 4)).join(", ") +
        "]"
      ;
    }
    return models.map(fmt);
  }

  public function fmtModels2 (): String {
    return models.map(m -> "" + Cts.modelIxs[m.modelId]).join("");
  }

  public static function fromJs (js: Js): Flea {
    final a = js.ra();
    return new Flea(
      a[0].ri(),
      a[1].ri(),
      a[2].rb(),
      a[3].ra().map(FModel.fromJs),
      a[4].rf()
    );
  }
}
