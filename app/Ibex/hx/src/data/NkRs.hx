// Copyright 21-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Nick results data.
class NkRs {
  /// Company nick or 'IBEX'.
  public final nick: String;
  /// Closes to calculate results.
  public final closes: Array<Float>;
  /// References to calculate results.
  public final refs: Array<Float>;
  /// Results with proportional intial capital.
  public final propResults: SimRs;
  /// Results with ponderated intial capital.
  public final pondResults: SimRs;

  function new (
    nick: String, closes: Array<Float>, refs: Array<Float>,
    propResults: SimRs, pondResults: SimRs
  ) {
    this.nick = nick;
    this.closes = closes;
    this.refs = refs;
    this.propResults = propResults;
    this.pondResults = pondResults;
  }

  public static function fromJs (js: Js): NkRs {
    final a = js.ra();
    return new NkRs(
      a[0].rs(),
      a[1].ra().map(e -> e.rf()),
      a[2].ra().map(e -> e.rf()),
      SimRs.fromJs(a[3]),
      SimRs.fromJs(a[4])
    );
  }
}
