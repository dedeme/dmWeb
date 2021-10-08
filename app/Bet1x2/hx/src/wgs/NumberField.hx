// Copyright 31-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Dec;
import I18n._;
import I18n._args;

/// Widget to enter currency amounts.
class NumberField {
  public final wg: Domo;
  public var value(default, null): Float;
  public final id: String;
  public final fn: (String, Float) -> Void;

  /// Creates a new widget.
  ///   id: Identifer used in 'fn' and as identifier of 'wg'.
  ///   targetId: Component id to pass focus.
  ///   value: Initial value.
  ///   fn: Function (id, value) that will be called when "onchange" happens.
  public function new (
    id: String, targetId: String, value: Float,
    fn: (String, Float) -> Void
  ) {
    this.value = value;
    this.id = id;
    this.fn = fn;
    wg = Ui.changePoint(Ui.field(targetId)
      .att("id", id)
      .style("width:80px")
      .value(Dec.to(value, 2).replace(".", ","))
      .on(CHANGE, onchange)
    );
    wg.on(FOCUS, cast(wg.e, js.html.InputElement).select);
  }

  function onchange () {
    final wgValue: String = wg.getValue().trim();
    final newValue = Std.parseFloat(wgValue.replace(".", "").replace(",", "."));
    if (Math.isNaN(newValue)) {
      Ui.alert(_args(_("'%0' is not a valid number"), [wgValue]));
      wg.value(Dec.to(value, 2).replace(".", ","));
      wg.e.focus();
    } else {
      value = newValue;
      fn(id, newValue);
    }
  }
}

