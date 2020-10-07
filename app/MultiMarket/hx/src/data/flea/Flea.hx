// Copyright 13-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.flea;

import dm.Js;

/// Flea record.
class Flea {
  public var date(default, null): String;
  public var cycle(default, null): Int;
  public var id(default, null): Int;
  public var params(default, null): Array<Float>;
  public var name(get, never): String;
  function get_name(): String {
    return date + "-" + Std.string(cycle) + "-" + Std.string(id);
  }

  function new (date: String, cycle: Int, id: Int, params: Array<Float>) {
    this.date = date;
    this.cycle = cycle;
    this.id = id;
    this.params = params;
  }

  public function toJs () {
    return Js.wa([
      Js.ws(date),
      Js.wi(cycle),
      Js.wi(id),
      Js.wa(params.map(e -> Js.wf(e)))
    ]);
  }

  public static function fromJs(js: Js): Flea {
    final a = js.ra();
    return new Flea(
      a[0].rs(),
      a[1].ri(),
      a[2].ri(),
      a[3].ra().map(e -> e.rf())
    );
  }
}
