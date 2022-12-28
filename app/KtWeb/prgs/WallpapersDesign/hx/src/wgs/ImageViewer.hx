// Copyright 23-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;

class ImageViewer {
  final wg: Domo;
  final url: String;
  final imgName: String;
  final width: Int;
  final height: Int;
  final onClick: Void -> Void;

  public function new (
    wg: Domo, url: String, width: Int, onClick: Void -> Void
  ) {
    this.wg = wg;
    this.url = url;
    this.width = width;
    this.onClick = onClick;
    height = Std.int(width * 9 / 16);
    imgName = url.substring(url.lastIndexOf("/") + 1);
  }

  public function show () {
    final img = Q("img")
      .att("src", url + "?t=" + Date.now().getTime())
      .att("title", imgName)
      .att("border", 1)
      .att("style", "cursor:pointer")
      .on(CLICK, onClick)
    ;
    img.on(LOAD, ev -> {
      final e = cast(img.e);
      if (e.width / e.height > 1.77777777) {
        img.att("width", width);
      } else {
        img.att("height", height);
      }
    });
    wg
      .removeAll()
      .add(img)
    ;
  }

}
