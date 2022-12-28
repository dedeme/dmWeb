// Copyright 23-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Opt;

/// Image data
class Image {
  public final id: String;
  public final cut: Option<ImgCut>;
  public final adjustment: Option<ImgAdjustment>;
  public final blur: Option<ImgBlur>;

  public function new (
    id: String, cut: Option<ImgCut>,
    adjustment: Option<ImgAdjustment>, blur: Option<ImgBlur>
  ) {
    this.id = id;
    this.cut = cut;
    this.adjustment = adjustment;
    this.blur = blur;
  }

  public function setCut (v: Option<ImgCut>): Image {
    return new Image(id, v, adjustment, blur);
  }

  public function setAdjustment (v: Option<ImgAdjustment>): Image {
    return new Image(id, cut, v, blur);
  }

  public function setBlur (v: Option<ImgBlur>): Image {
    return new Image(id, cut, adjustment, v);
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(id),
      switch (cut) { case Some(v): v.toJs(); case None: Js.wn(); },
      switch (adjustment) { case Some(v): v.toJs(); case None: Js.wn(); },
      switch (blur) { case Some(v): v.toJs(); case None: Js.wn(); }
    ]);
  }

  public static function fromJs (js: Js): Image {
    final a = js.ra();
    return new Image(
      a[0].rs(),
      a[1].isNull() ? None : Some(ImgCut.fromJs(a[1])),
      a[2].isNull() ? None : Some(ImgAdjustment.fromJs(a[2])),
      a[3].isNull() ? None : Some(ImgBlur.fromJs(a[3]))
    );
  }
}
