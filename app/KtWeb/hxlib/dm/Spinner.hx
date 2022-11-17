// Copyright 14-Nov-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import dm.Domo;
import dm.Ui.Q;

enum SpinnerSize { TINY; SMALL; NORMAL; BIG; LARGE; }

/// Number spinner.
class Spinner {
  /// Widget type "table"
  public final wg = Q("table").style("border-collapse : collapse;");
  final moreWg = Q("span");
  final lessWg = Q("span");
  final valueWg = Q("span");

  var sz: String;
  var width: String;
  var tm = Date.now();
  /// Minimum value (Between -100 and 99, both inclusive)
  public var min(default, null): Int;
  /// Maximum value (Between -99 and 100, both inclusive)
  public var max(default, null): Int;
  /// Current spinner value.
  public var value(default, null): Int;
  /// Function to run when 'value' change.
  public var onChange: Int -> Void = v -> {};

  /// Constructor.
  ///
  /// Constraints:
  ///   * max > min
  ///   * value >= min and value <= max
  ///
  ///   min: Minimum value (Between -100 and 99, both inclusive)
  ///   max: Maximum value (Between -99 and 100, both inclusive)
  ///   value: Initial value.
  ///   size: Value for css "font-size"
  public function new (min: Int, max: Int, value: Int, ?size: SpinnerSize) {
    if (max < -99) throw new haxe.Exception ('max ($max) < -99');
    if (min > 99) throw new haxe.Exception ('min ($min) > 99');
    if (max <= min) throw new haxe.Exception ('max ($max) <= min ($min)');
    if (value < min) throw new haxe.Exception ('value ($value) < min ($min)');
    if (value > max) throw new haxe.Exception ('value ($value) > max ($max)');

    this.max = max;
    this.min = min;
    this.value = value;

    size = size == null ? NORMAL : size;
    switch (size) {
      case TINY:
        sz = "x-small";
        width = "40px";
      case SMALL:
        sz = "small";
        width = "45px";
      case NORMAL:
        sz = "normal";
        width = "50px";
      case BIG:
        sz = "medium";
        width = "55px";
      case LARGE:
        sz = "large";
        width = "60px";
    }

    wg
      .add(Q("tr")
        .add(Q("td")
          .style(
            "border : 1px solid rgb(110,130,150);" +
            'font-size:$sz;'
          )
          .add(lessWg))
        .add(Q("td")
          .style(
            'width:$width;font-size:$sz;' +
            "text-align: center;" +
            "border : 1px solid rgb(110,130,150);" +
            "background-color : rgb(250, 250, 250);"
          )
          .on(WHEEL, wheel)
          .add(valueWg))
        .add(Q("td")
          .style(
            "border : 1px solid rgb(110,130,150);" +
            'font-size:$sz;'
          )
          .add(moreWg)))
    ;

    update();
  }

  // View ----------------------------------------------------------------------

  function update () {
    if (value == min) {
      lessWg
        .removeAll()
        .html("&#x25C0;")
      ;
    } else {
      lessWg
        .removeAll()
        .add(Ui.link(lessFn)
          .html("&#x25C0;"))
      ;
    }
    valueWg
      .removeAll()
      .text(Std.string(value))
    ;
    if (value == max) {
      moreWg
        .removeAll()
        .html("&#x25B6;")
      ;
    } else {
      moreWg
        .removeAll()
        .add(Ui.link(moreFn)
          .html("&#x25B6;"))
      ;
    }
  }

  // Control -------------------------------------------------------------------

  function lessFn (e: js.html.MouseEvent): Void {
    --value;
    onChange(value);
    update();
  }

  function moreFn (e: js.html.MouseEvent): Void {
    ++value;
    onChange(value);
    update();
  }

  function wheel (e: js.html.WheelEvent): Void {
    final inc = e.deltaY > 0 ? -1 : 1;
    final old = value;
    value = value + inc;
    if (value > max) value = max;
    if (value < min) value = min;
    if (value == old) return;
    update();

    tm = Date.now();
    haxe.Timer.delay(
      () -> {
        if (Dt.dfMillis(Date.now(), tm) > 150) {
          tm = Date.now();
          onChange(value);
        }
      },
      200
    );
  }

}

