// Copyright 23-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Image 'cut' data
class ImgCut {
  /// left: Left point to cut.
  public final left: Int;
  /// top: Top point to cut.
  public final top: Int;
  /// right: Right point to cut.
  public final right: Int;
  /// bottom: Bottom point to cut.
  public final bottom: Int;

  public function new (left: Int, top: Int, right: Int, bottom: Int) {
    this.left = left;
    this.top = top;
    this.right = right;
    this.bottom = bottom;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(left),
      Js.wi(top),
      Js.wi(right),
      Js.wi(bottom)
    ]);
  }

  public static function fromJs (js: Js): ImgCut {
    final a = js.ra();
    return new ImgCut(
      a[0].ri(),
      a[1].ri(),
      a[2].ri(),
      a[3].ri()
    );
  }
}
