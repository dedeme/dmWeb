// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;

/// Parameter widget.
class Param {
  var name: String;
  var min: Float;
  var max: Float;
  var inp: Domo;
  public final wg = Q("div");

  public var value(get, never): Float;
  public function get_value() {
    switch Dec.fromIso(cast(inp.getValue(), String).trim()) {
      case Some(v): return v;
      case None:
        Ui.alert(cast(inp.getValue(), String) + " is not a valid number");
        return min;
    }
  }

  public function new (
    name: String, min: Float, max: Float,
    id: String, nextId: String, ?value: Float
  ) {
    this.name = name;
    this.min = Dec.round(min, 6);
    this.max = Dec.round(max, 6);

    inp = Ui.field(nextId)
      .att("id", id)
      .style("width:60px")
      .on(CHANGE, e -> onChange())
      .value(value == null
        ? Dec.toIso((min + max) / 2, 6)
        : Dec.toIso(value, 6)
      )
    ;
    Ui.changePoint(inp);

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    function label () {
      return Q("div").style(
        "text-align:center;color:#c9c9c9;font-style:italic"
      );
    }
    wg
      .removeAll()
      .add(Q("div").style("text-align:center").text(name))
      .add(label().text(Dec.toIso(min, 6)))
      .add(inp)
      .add(label().text(Dec.toIso(max, 6)))
    ;
  }

  // Control -------------------------------------------------------------------

  function onChange () {
    final v = get_value();
    if (v < min) inp.value(Dec.toIso(min, 6));
    if (v > max) inp.value(Dec.toIso(max, 6));
    inp.value(Dec.toIso(v, 6));
  }
}
