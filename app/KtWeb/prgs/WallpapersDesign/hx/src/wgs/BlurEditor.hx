// Copyright 25-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import data.Image;
import data.ImgBlur;
import I18n._;

/// Editor of blur operation.
class BlurEditor {
  final imgDiv: Domo;
  final image: Image;
  final onChange: Image -> Void;

  final ratio: Domo;
  final editorDiv = Q("div");

  var isActivated = false;

  public function new (imgDiv: Domo, image: Image, onChange: Image -> Void) {
    this.imgDiv = imgDiv;
    this.image = image;
    this.onChange = onChange;
    final blur = Opt.get(image.blur);
    isActivated = blur != null;

    function mkIn (id: String, nextId: String, value: Int) {
      return Ui.changePoint(Ui.field(nextId)
        .att("id", id)
        .style("width:80px")
        .value(value)
      );
    }
    final df = Cts.pixelsCut;
    ratio = mkIn("ratio", "ratio", isActivated ? blur.ratio : Cts.ratioBlur);

    mkEditor(editorDiv);
    show();
  }

  function show (): Void {
    Cts.boxContent
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td"))
          .add(Q("td")
            .style("text-align:right")
            .add(Ui.link(close)
              .klass("link")
              .text(_("Close")))))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:left")
            .add(Ui.link(update)
              .klass("link")
              .text(_("Update")))
            .add(Q("span")
              .html("&nbsp;&nbsp;"))
            .add(Ui.link(restore)
              .klass("link")
              .text(_("Restore"))))
          .add(Q("td"))))
      .add(Q("hr"))
      .add(editorDiv)
      .add(Q("hr"))
      .add(imgDiv)
    ;
  }

  function mkEditor (div: Domo) {
    ratio.disabled(!isActivated);

    div
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 3)
            .add(isActivated
                ? Ui.link(() -> activate(false))
                    .klass("link")
                    .text(_("Deactivate"))
                : Ui.link(() -> activate(true))
                    .klass("link")
                    .text(_("Activate"))
              )))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .text(_("Percentage (0 - 100)"))))
        .add(Q("tr")
          .add(Q("td")
            .add(ratio))))
    ;
  }

  // Control -------------------------------------------------------------------

  function restore () {
    final blur = Opt.get(image.blur);
    isActivated = blur != null;
    ratio.value(isActivated ? blur.ratio : Cts.ratioBlur);

    onChange(image);
    mkEditor(editorDiv);
  }

  function update () {
    function value (i : Domo): Int {
      var r = Std.parseInt(i.getValue());
      if (r == null) {
        r = Cts.ratioBlur;
        i.value("" + r);
      }
      return r;
    }
    final blur = isActivated
      ? Some(new ImgBlur(value(ratio)))
      : None
    ;

    onChange(image.setBlur(blur));
    mkEditor(editorDiv);
  }

  function close () {
    Cts.box.show(false);
  }

  function activate (value: Bool): Void {
    isActivated = value;
    mkEditor(editorDiv);
  }
}
