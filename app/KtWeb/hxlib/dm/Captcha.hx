// Copyright 16-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import dm.Domo;
import dm.Ui.Q;
import dm.Store;
import dm.It;
import dm.Rnd;

/// Captcha widget.
class Captcha {
  var storeId(default, null): String;
  var counterLimit(default, null): Int;
  var zeroColor(default, null): String;
  var oneColor(default, null): String;

  var counter: Int;

  final ch0 = It.range(4).map(_ -> Q("input").att("type", "checkbox")).to();
  final ch1 = It.range(4).map(_ -> Q("input").att("type", "checkbox")).to();
  /// Captcha widget.
  public var wg = Q("div");

  /// Constructor.
  ///   storeId     : Identifier for store Captcha data in local store.
  ///   counterLimit: Maximun error number without captcha.
  ///   zeroColor   : Color cells to not mark (Default: "#f0f0f0").
  ///   oneColor    : Color cells to mark (Default: "#c0c0c0").
  public function new (
    storeId: String, counterLimit: Int = 3,
    zeroColor: String = "#f0f0f0", oneColor: String = "#c0c0c0"
  ) {
    this.storeId = storeId;
    this.counterLimit = counterLimit;
    this.zeroColor = zeroColor;
    this.oneColor = oneColor;
    final now = Date.now().getTime();
    counter = getCounter(storeId);
    if (now - getTime(storeId) > 900000) {
      counter = 0;
      setCounter(storeId, 0);
      setTime(storeId, Date.now().getTime());
    }

    this.view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    final tds = It.from(ch0).map(ch ->
      Q("td")
        .att("style", "border: 1px solid;background-color: " + zeroColor)
        .add(ch)
    ).cat(It.from(ch1).map(ch ->
      Q("td")
        .att("style", "border: 1px solid;background-color: " + oneColor)
        .add(ch)
    )).to();
    final box = new Box(tds);
    final tds1 = It.range(4).map(_ -> box.next()).to();
    final tds2 = It.range(4).map(_ -> box.next()).to();

    wg
      .removeAll()
      .add(Q("table")
        .att("border", 0)
        .style("border: 1px solid;background-color: #fffff0")
        .add(Q("tr")
          .adds(tds1))
        .add(Q("tr")
          .adds(tds2)))
    ;
  }

  // Control -------------------------------------------------------------------

  /// Returns true if tries counter is greater or equals to its limit.
  public function isUpLimit (): Bool {
    return counter >= counterLimit;
  }

  /// Checks cells.
  public function check (): Bool {
    return It.from(ch0).every(ch -> !ch.getChecked()) &&
      It.from(ch1).every(ch -> ch.getChecked());
  }

  /// Increments counter.
  public function increment (): Void {
    setCounter(storeId, counter + 1);
    setTime(storeId, Date.now().getTime());
  }

  /// Resets counter.
  public function reset () {
    resetCounter(storeId);
    resetTime(storeId);
  }

  // Static --------------------------------------------------------------------

  static function getCounter (id: String): Int {
    return switch (Store.get(id + "_counter")) {
      case Some(v): Std.parseInt(v);
      case None: 0;
    }
  }

  static function setCounter (id: String, n: Int): Void {
    Store.put(id + "_counter", Std.string(n));
  }

  static function resetCounter (id: String): Void {
    Store.del(id + "_counter");
  }

  static function getTime (id: String): Float {
    return switch (Store.get(id + "_time")) {
      case Some(v): Std.parseFloat(v);
      case None: Date.now().getTime();
    }
  }

  static function setTime (id: String, n: Float): Void {
    Store.put(id + "_time", Std.string(n));
  }

  static function resetTime (id: String): Void {
    Store.del(id + "_time");
  }
}
