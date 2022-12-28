// Copyright 24-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import data.Image;
import data.ImgCut;
import I18n._;

/// Editor of cut operation.
class CutEditor {
  final imgDiv: Domo;
  final image: Image;
  final onChange: Image -> Void;

  final left: Domo;
  final top: Domo;
  final right: Domo;
  final bottom: Domo;
  final editorDiv = Q("div");

  var isActivated = false;

  public function new (imgDiv: Domo, image: Image, onChange: Image -> Void) {
    this.imgDiv = imgDiv;
    this.image = image;
    this.onChange = onChange;
    final cut = Opt.get(image.cut);
    isActivated = cut != null;

    function mkIn (id: String, nextId: String, value: Int) {
      final r = Ui.changePoint(Ui.field(nextId)
        .att("id", id)
        .style("width:80px")
        .value(value)
      );
      return r;
    }
    final df = Cts.pixelsCut;
    top = mkIn("top", "right", isActivated ? cut.top : df);
    left = mkIn("left", "top", isActivated ? cut.left : df);
    bottom = mkIn("bottom", "left", isActivated ? cut.bottom : df);
    right = mkIn("right", "bottom", isActivated ? cut.right : df);

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
    left.disabled(!isActivated);
    top.disabled(!isActivated);
    right.disabled(!isActivated);
    bottom.disabled(!isActivated);

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
            .text(_("Left")))
          .add(Q("td"))
          .add(Q("td")
            .text(_("Top"))))
        .add(Q("tr")
          .add(Q("td")
            .add(left))
          .add(Q("td"))
          .add(Q("td")
            .add(top)))
        .add(Q("tr")
          .add(Q("td")
            .text(_("Right")))
          .add(Q("td"))
          .add(Q("td")
            .text(_("Bottom"))))
        .add(Q("tr")
          .add(Q("td")
            .add(right))
          .add(Q("td"))
          .add(Q("td")
            .add(bottom))))
    ;
  }

  // Control -------------------------------------------------------------------

  function restore () {
    final cut = Opt.get(image.cut);
    isActivated = cut != null;
    final df = Cts.pixelsCut;
    top.value(isActivated ? cut.top : df);
    left.value(isActivated ? cut.left : df);
    bottom.value(isActivated ? cut.bottom : df);
    right.value(isActivated ? cut.right : df);

    onChange(image);
    mkEditor(editorDiv);
  }

  function update () {
    function value (i : Domo): Int {
      var r = Std.parseInt(i.getValue());
      if (r == null) {
        r = Cts.pixelsCut;
        i.value("" + r);
      }
      return r;
    }
    final cut = isActivated
      ? Some(new ImgCut(
          value(left), value(top), value(right), value(bottom)
        ))
      : None
    ;

    onChange(image.setCut(cut));
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
