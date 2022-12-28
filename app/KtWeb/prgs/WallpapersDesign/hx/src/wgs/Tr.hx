// Copyright 23-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import data.Image;
import wgs.ImageViewer;
import wgs.CutEditor;
import wgs.AdjustmentEditor;
import wgs.BlurEditor;
import I18n._;
import I18n._args;

/// Images row.
class Tr {
  final images: Array<Image>;
  final ix: Int;
  var image: Image;
  final optionsDiv = Q("div");
  final sourceDiv = Q("div");
  final targetDiv = Q("div");
  final imgEditorDiv = Q("div");

  public function new (images: Array<Image>, ix: Int) {
    this.images = images;
    this.ix = ix;
    image = images[ix];
  }

  public function mkTr() {
    update();
    return Q("tr")
      .add(Q("td")
        .style("text-align:left;vertical-align:middle")
        .add(optionsDiv))
      .add(Q("td").klass("separator"))
      .add(Q("td")
        .style("text-align:center;vertical-align:middle")
        .add(sourceDiv))
      .add(Q("td").klass("separator"))
      .add(Q("td")
        .style("text-align:center;vertical-align:middle")
        .add(targetDiv))
    ;
  }

  public function update (): Void {
    showSourceImg();
    showTarget();
  }

  function updateImage (img: Image): Void {
    image = img;
    images[ix] = img;
    Global.client.ssend([
      "prg" => Js.ws(Cts.appName),
      "source" => Js.ws("Tr"),
      "rq" => Js.ws("update"),
      "images" => Js.wa(images.map(i -> i.toJs()))
    ], rp -> {
      showTarget();
    });
  }

  function showSourceImg (): Void {
    final action = () -> {
      new ImageViewer(
        Cts.boxContent, "source/" + image.id, 800,
        () -> Cts.box.show(false)
      ).show();
      Cts.box.show(true);
    };

    new ImageViewer(sourceDiv, "source/" + image.id, 240, action).show();
  }

  function showOptions (): Void {
    final action = img -> {};

    optionsDiv
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("input")
              .att("type", "checkbox")
              .disabled(true)
              .checked(Opt.get(image.cut) != null)))
          .add(Q("td")
            .add(Ui.link(editCut)
              .klass("link")
              .text(_("Cut")))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("input")
              .att("type", "checkbox")
              .disabled(true)
              .checked(Opt.get(image.adjustment) != null)))
          .add(Q("td")
            .add(Ui.link(adjustmentCut)
              .klass("link")
              .text(_("Adjustment")))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("input")
              .att("type", "checkbox")
              .disabled(true)
              .checked(Opt.get(image.blur) != null)))
          .add(Q("td")
            .add(Ui.link(editBlur)
              .klass("link")
              .text(_("Blur")))))
      )
    ;
  }

  function showTargetImg (): Void {
    final ix = image.id.lastIndexOf(".");
    final newId = ix == -1
      ? image.id + ".jpg"
      : image.id.substring(0, ix) + ".jpg"
    ;

    final action = () -> {
      new ImageViewer(
        Cts.boxContent, "target/" + newId, 800,
        () -> Cts.box.show(false)
      ).show();
      Cts.box.show(true);
    };

    new ImageViewer(targetDiv, "target/" + newId, 240, action).show();
    new ImageViewer(imgEditorDiv, "target/" + newId, 800, () -> {}).show();
  }

  function showTarget (): Void {
    optionsDiv.removeAll().add(Ui.img("wait.gif"));
    targetDiv.removeAll().add(Ui.img("wait.gif"));
    imgEditorDiv.removeAll().add(Ui.img("wait.gif"));
    Global.client.send([
      "prg" => Js.ws(Cts.appName),
      "source" => Js.ws("Tr"),
      "rq" => Js.ws("idata"),
      "image" => image.toJs()
    ], rp -> {
      if (!rp["ok"].rb()) {
        Ui.alert(_args(_("Image '%0' can not be processed"), [image.id]));
        js.Browser.location.assign("");
      } else {
        showOptions();
        showTargetImg();
      }
    });
  }

  // Control -------------------------------------------------------------------

  function editCut (): Void {
    new CutEditor(imgEditorDiv, image, updateImage);
    Cts.box.show(true);
  }

  function adjustmentCut (): Void {
    new AdjustmentEditor(imgEditorDiv, image, updateImage);
    Cts.box.show(true);
  }

  function editBlur (): Void {
    new BlurEditor(imgEditorDiv, image, updateImage);
    Cts.box.show(true);
  }

}
