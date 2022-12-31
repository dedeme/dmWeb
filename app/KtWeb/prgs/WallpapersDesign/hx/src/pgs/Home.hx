// Copyright 23-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import data.Image;
import data.Dim;
import wgs.Tr;
import wgs.DimSelector;
import I18n._;
import I18n._args;

/// Home page.
class Home {
  final wg: Domo;
  final images: Array<Image>;
  var group = "0";

  function new (wg: Domo, images: Array<Image>) {
    this.wg = wg;
    this.images = images;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    if (images.length == 0) {
      wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
            .klass("frame")
            .text(_("There are no pictures to adjust")))))
      ;
      return;
    }

    final tdSave = Q("td")
      .klass("frame")
      .style("text-aling:left;width:5px;white-space:nowrap")
    ;
    final tdDim = Q("td")
      .klass("frame")
      .style("text-align:right;width:5px;white-space:nowrap")
    ;
    final trs = [];
    trs.push(Q("tr")
      .add(Q("td")
        .att("colspan", 5)
        .style(
            "pading-top:15px;" +
            "pading-bottom:10px"
          )
        .add(Q("table")
          .klass("main")
          .add(Q("tr")
            .add(tdSave)
            .add(Q("td"))
            .add(tdDim))))
    );
    trs.push(Q("tr")
      .add(Q("td")
        .att("colspan", 5)
        .add(Q("hr")))
    );

    mkSave(tdSave);
    mkDim(tdDim);
    for (ix in 0...images.length) {
      trs.push(new Tr(images, ix).mkTr());
    }

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .adds(trs))
      .add(Cts.box.wg)
    ;
  }

  // Control -------------------------------------------------------------------

  function mkSave (td: Domo): Void {
    td
      .removeAll()
      .add(Ui.img("wait.gif"));
    Global.client.send([
      "prg" => Js.ws(Cts.appName),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata2")
    ], rp -> {
      final duplicates = rp["duplicates"].ra().map(d -> d.rs());
      group = rp["group"].rs();

      if (duplicates.length == 0) {
        td
          .removeAll()
          .add(Ui.link(save)
            .klass("link")
            .text(_args(_("Save in Group %0"), [group])))
        ;
      } else {
        td
          .removeAll()
          .html("<p>" + _("There are duplicated pictures") + ":</p><p>" +
              duplicates.join("<br>") + "</p>")
        ;
      }
    });
  }

  function mkDim (td: Domo): Void {
    td
      .removeAll()
      .add(Ui.img("wait.gif"));
    Global.client.send([
      "prg" => Js.ws(Cts.appName),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("dim")
    ], rp -> {
      final width = rp["width"].ri();
      final height = rp["height"].ri();
      final dim = "" + width + " x " + height;

      td
        .removeAll()
        .add(Ui.link(() -> changeDim(dim))
          .klass("link")
          .text(dim))
      ;
    });
  }

  function save (): Void {
    if (Ui.confirm(_args(_("Save pictures in group %0?"), [group]))) {
      Cts.boxContent
        .removeAll()
        .add(Ui.img("wait.gif"))
      ;
      Cts.box.show(true);
      Global.client.send([
        "prg" => Js.ws(Cts.appName),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("save"),
        "group" => Js.ws(group)
      ], rp -> {
        js.Browser.location.assign("");
      });
    }
  }

  function changeDim (dim: String): Void {
    function cancel (): Void {
      Cts.box.show(false);
    }
    function accept (dim: Dim): Void {
      Global.client.send([
        "prg" => Js.ws(Cts.appName),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("changeDim"),
        "width" => Js.wi(dim.width),
        "height" => Js.wi(dim.height),
      ], rp -> {
        js.Browser.location.assign("");
      });
    }

    new DimSelector(Cts.boxContent, dim, cancel, accept).show();
    Cts.box.show(true);
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws(Cts.appName),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final images = rp["images"].ra().map(Image.fromJs);
      new Home(wg, images).show();
    });
  }
}
