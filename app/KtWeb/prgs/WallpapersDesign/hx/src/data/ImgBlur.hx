// Copyright 23-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Image 'blur' data
class ImgBlur {
  /// ratio: Value between 0 and 100 (both inclusive).
  public final ratio: Int;

  public function new (ratio: Int) {
    this.ratio = ratio;
  }

  public function toJs (): Js {
    return Js.wi(ratio);
  }

  public static function fromJs (js: Js): ImgBlur {
    return new ImgBlur(js.ri());
  }
}
