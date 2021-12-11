// Copyright 03-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import data.Cts;
import I18n._;

/// Parameter widget.
class Param {
  var name: String;
  var inp: Domo;
  public final wg = Q("div");

  public var value(get, never): Float;
  public function get_value() {
    switch Dec.fromIso(cast(inp.getValue(), String).trim()) {
      case Some(v): return v;
      case None:
        Ui.alert(cast(inp.getValue(), String) + " is not a valid number");
        return Cts.rangesMedium / 100;
    }
  }

  public function new (
    name: String, id: String, nextId: String, ?value: Float
  ) {
    this.name = name;

    inp = Ui.field(nextId)
      .att("id", id)
      .style("width:80px")
      .on(CHANGE, e -> onChange())
      .value(value == null
        ? Dec.toIso(Cts.rangesMedium / 100, Cts.paramDecs)
        : Dec.toIso(value, Cts.paramDecs)
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
    final rangesMin = Cts.rangesMin /100;
    final rangesMax = (Cts.rangesMin + Cts.rangesGroups) / 100;
    wg
      .removeAll()
      .add(Q("div").style("text-align:center").text(name))
      .add(label().text(">= " + Dec.toIso(rangesMin, Cts.paramDecs)))
      .add(inp)
      .add(label().text("< " + Dec.toIso(rangesMax, Cts.paramDecs)))
    ;
  }

  // Control -------------------------------------------------------------------

  function onChange () {
    final rangesMin = Cts.rangesMin /100;
    final rangesMax = (Cts.rangesMin + Cts.rangesGroups) / 100;

    final v = get_value();
    if (v < rangesMin) {
      Ui.alert(_("Value less than minimum"));
      inp.value(Dec.toIso(Cts.rangesMedium / 100, Cts.paramDecs));
      return;
    }
    if (v >= rangesMax) {
      Ui.alert(_("Value greater or equals to maximum"));
      inp.value(Dec.toIso(Cts.rangesMedium / 100, Cts.paramDecs));
      return;
    }
    inp.value(Dec.toIso(v, Cts.paramDecs));
  }
}
