// Copyright 23-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Image 'adjustment' data
class ImgAdjustment {
  /// type: Adjustment type. One of "cut", "background" or "stretch".
  public final type: String;
  /// params: Parameters wich meaning depenends on type.
  ///         For "cut":
  ///           - Top value (pixels).
  ///         For "background":
  ///           - Background color in format RGB.
  ///           - Ratio of blurriness. Value between 0 and 100 (both inclusive).
  ///           - Ratio of light. Value between 0 and 100 (both inclusive).
  ///         For "stretch":
  ///           - Pixels between 1 and 100 (both inclusive).
  public final params: Array<Int>;

  public function new (type: String, params: Array<Int>) {
    this.type = type;
    this.params = params;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(type),
      Js.wa(params.map(Js.wi))
    ]);
  }

  public static function fromJs (js: Js): ImgAdjustment {
    final a = js.ra();
    return new ImgAdjustment(
      a[0].rs(),
      a[1].ra().map(p -> p.ri())
    );
  }
}
