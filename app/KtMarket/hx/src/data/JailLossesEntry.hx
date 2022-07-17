// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Profits entry data.
class JailLossesEntry {
  public final date: String;
  public final losses: Float;

  /// Constructor.
  ///   date  : Data date.
  ///   losses: Jail losses (a positive number).
  public function new (date: String, losses: Float) {
    this.date = date;
    this.losses = losses;
  }

  public static function fromJs (js: Js): JailLossesEntry {
    final a = js.ra();
    return new JailLossesEntry(
      a[0].rs(),
      a[1].rf()
    );
  }
}
