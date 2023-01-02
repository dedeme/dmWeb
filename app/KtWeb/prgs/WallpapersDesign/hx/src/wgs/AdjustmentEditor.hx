// Copyright 25-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import data.Image;
import data.ImgBlur;
import data.ImgAdjustment;
import I18n._;

/// Editor of adjustment operations.
class AdjustmentEditor {
  final imgDiv: Domo;
  final image: Image;
  final onChange: Image -> Void;

  final startCut: Domo;
  final ratioBlur: Domo;
  final ratioLight: Domo;
  final pixelsStretch: Domo;
  final color: Domo;
  final inputsDiv = Q("div");
  final editorDiv = Q("div");

  var state = ""; // State deactivated.

  public function new (imgDiv: Domo, image: Image, onChange: Image -> Void) {
    this.imgDiv = imgDiv;
    this.image = image;
    this.onChange = onChange;
    final adj = Opt.get(image.adjustment);
    state = adj == null ? "" : adj.type;

    function mkIn (id: String, nextId: String, value: Int) {
      return Ui.changePoint(Ui.field(nextId)
        .att("id", id)
        .style("width:80px")
        .value(value)
      );
    }

    startCut = mkIn(
      "startCut", "startCut",
      state == "cut" ? adj.params[0] : 0
    );
    ratioBlur = mkIn(
      "ratioBlur", "ratioLight",
      state == "background" ? adj.params[1] : 50
    );
    ratioLight = mkIn(
      "ratioLight", "ratioBlur",
      state == "background" ? adj.params[2] : 30
    );
    pixelsStretch = mkIn(
      "pixelsStretch", "pixelsStretch",
      state == "stretch" ? adj.params[0] : 10
    );

    color = Q("input")
      .att("type", "color")
      .value(state == "background"
          ? "#" + Fns.intToColor(adj.params[0])
          : "#ffffff"
        )
    ;

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
    final deactivateBt = Q("input")
      .att("type", "radio")
      .att("name", "type")
      .checked(state == "")
      .on(CLICK, () -> setState(""))
    ;
    final cutBt = Q("input")
      .att("type", "radio")
      .att("name", "type")
      .checked(state == "cut")
      .on(CLICK, () -> setState("cut"))
    ;
    final backgroundBt = Q("input")
      .att("type", "radio")
      .att("name", "type")
      .checked(state == "background")
      .on(CLICK, () -> setState("background"))
    ;
    final stretchBt = Q("input")
      .att("type", "radio")
      .att("name", "type")
      .checked(state == "stretch")
      .on(CLICK, () -> setState("stretch"))
    ;
    final left = Q("div")
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(deactivateBt))
          .add(Q("td")
            .style("text-align:left")
            .text(_("Deactivate"))))
        .add(Q("tr")
          .add(Q("td")
            .add(cutBt))
          .add(Q("td")
            .style("text-align:left")
            .text(_("Cut"))))
        .add(Q("tr")
          .add(Q("td")
            .add(backgroundBt))
          .add(Q("td")
            .style("text-align:left")
            .text(_("Background"))))
        .add(Q("tr")
          .add(Q("td")
            .add(stretchBt))
          .add(Q("td")
            .style("text-align:left")
            .text(_("Stretch"))))
      )
    ;
    final right = Q("div");
    if (state == "cut") {
      right
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .text(_("Pixels from top / left"))))
          .add(Q("tr")
            .add(startCut)))
      ;
    } else if (state == "background") {
      right
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .text(_("Color")))
            .add(Q("td")
              .text(_("Blur (0 - 100)")))
            .add(Q("td")
              .text(_("Light (0 - 100)"))))
          .add(Q("tr")
            .add(Q("td")
              .add(color))
            .add(Q("td")
              .add(ratioBlur))
            .add(Q("td")
              .add(ratioLight))))
      ;
    } else if (state == "stretch") {
      right
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .text(_("Pixels to sample"))))
          .add(Q("tr")
            .add(pixelsStretch)))
      ;
    } else {
      right.removeAll();
    }

    div
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;text-align:left")
            .add(left))
          .add(Q("td")
            .add(right))))
    ;
  }

  // Control -------------------------------------------------------------------

  function restore () {
    final adj = Opt.get(image.adjustment);
    state = adj == null ? "" : adj.type;

    startCut.value(state == "cut" ? adj.params[0] : 0);
    ratioBlur.value(state == "background" ? adj.params[1] : 50);
    ratioLight.value(state == "background" ? adj.params[2] : 30);
    pixelsStretch.value(state == "stretch" ? adj.params[0] : 10);
    color.value(
      state == "background" ? Fns.intToColor(adj.params[0]) : "#ffffff"
    );

    onChange(image);
    mkEditor(editorDiv);
  }

  function update () {
    function value (i : Domo, min: Int, max: Int): Int {
      var r = Std.parseInt(i.getValue());
      if (r == null) {
        r = Cts.ratioBlur;
        i.value("" + r);
      }
      return r;
    }
    final adj = switch (state) {
      case "cut":
        Some(new ImgAdjustment(state, [value(startCut, 0, 1000000)]));
      case "background":
        Some(new ImgAdjustment(state, [
          Fns.colorToInt(color.getValue().substring(1)),
          value(ratioBlur, 0, 100),
          value(ratioLight, 0, 1000000)
        ]));
      case "stretch":
        Some(new ImgAdjustment(state, [value(pixelsStretch, 1, 100)]));
      default: None;
    }

    onChange(image.setAdjustment(adj));
    mkEditor(editorDiv);
  }

  function close () {
    Cts.box.show(false);
  }

  function setState (v: String) {
    state = v;
    mkEditor(editorDiv);
  }

}
