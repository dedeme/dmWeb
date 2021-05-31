// Copyright 18-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package widgets;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Clock;

// Clocks widget
class Clocks {
  public final clockWg: Domo;
  public final chronWg: Domo;

  final wg: Domo;

  public function new (wg: Domo) {
    this.wg = wg;
    final clock = new Clock();
    clock.width *= 2;
    clock.height *= 2;
    clockWg = clock.wg;
    clockWg.style(
      "opacity:0;" +
      "transition: opacity 2s linear;"
    );
    final chron = new Clock(true);
    chron.width *= 2;
    chron.height *= 2;
    chronWg = chron.wg;
    chronWg.style(
      "background:radial-gradient(#000333,#e6f6f6);" +
      "border: 1px solid rgb(110,130,150);" +
      "border-radius: 44px;" +
      "opacity:0;" +
      "transition: opacity 2s linear;"
    );
    view ();
  }

  // VIEW
  function view () {
    wg
      //.removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(clockWg))
          .add(Q("td")
            .add(chronWg))))
    ;
  }

  // CONTROL

  /// Change opacity to "0" or to '1'.
  public function clockChangeOpacity (): Void {
    final isHidden = clockWg.e.style.getPropertyValue("opacity") == "0";
    clockWg.setStyle("opacity", isHidden ? "1" : "0");
  }

  /// Change opacity to "0" or to '1'.
  public function chronChangeOpacity (): Void {
    final isHidden = chronWg.e.style.getPropertyValue("opacity") == "0";
    chronWg.setStyle("opacity", isHidden ? "1" : "0");
  }

  /// Change background of chronometer
  public function chronOutOfTime () {
    chronWg.setStyle("background", "radial-gradient(#330300,#f6f6e6)");
  }

}
