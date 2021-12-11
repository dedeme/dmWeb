// Copyright 20-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;

import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.Str;
import dm.It;
import dm.Js;
import dm.Opt;
import dm.Menu;
import I18n._;
import I18n._args;
import I18n;
import cm.data.Conf;
import cm.data.Paths;
import cm.data.Lpath;

/// Paths page.
class PathsPg {
  final wg: Domo;
  final conf: Conf;
  final paths: Paths;
  final mkMenu: Paths -> Void;

  var modifyPath: Option<Lpath>;

  final libIn: Domo;
  final pathIn: Domo;
  final libMd: Domo;
  final pathMd: Domo;

  /// Constructor.
  ///   wg    : Container.
  ///   conf  : Settings data.
  ///   paths : Paths list.
  ///   mkMenu: Function to make application menu.
  public function new (
    wg: Domo, conf: Conf, paths: Paths, mkMenu: Paths -> Void
  ) {
    this.wg = wg;
    this.conf = conf;
    this.paths = paths;
    this.mkMenu = mkMenu;

    modifyPath = None;

    libIn = Ui.field("pathIn").att("id", "autofocus").att("size", 20);
    pathIn = Ui.field("newEnterBt").att("id", "pathIn").att("size", 60);
    libMd = Ui.field("pathMd").att("id", "libMd").att("size", 20);
    pathMd = Ui.field("libMd").att("id", "pathMd").att("size", 60);

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {

    if (conf.lang == "es") I18n.es();
    else I18n.en();

    switch (modifyPath) {
    case Some(pt):
      wg
        .removeAll()
        .add(Q("h2")
          .att("align", "center")
          .html(_("Libraries")))
        .add(Q("table")
          .att("border", 0)
          .att("align", "center")
          .klass("border")
          .style("background-color: rgb(255, 250, 250)")
          .add(Q("tr")
            .add(Q("td")
              .add(Ui.img("new")
                .style("vertical-align:-15%")))
            .add(Q("td")
              .att("align", "center")
              .att("colspan", 2)
              .add(Q("button")
                .disabled(true)
                .style("width: 70px")
                .att("id", "newEnterBt")
                .on(CLICK, addPath)
                .add(Ui.img("enter").style("vertical-align:-10%"))))
            .add(Q("td")
              .add(libIn.disabled(true)))
            .add(Q("td")
              .add(pathIn.disabled(true))))
          .add(Q("tr")
            .add(Q("td")
              .style("width:18px")
              .add(Ui.lightImg(conf.show ? "out" : "in")))
            .add(Q("td")
              .style("width:18px")
              .add(Ui.img("blank")))
            .add(Q("td")
              .style("width:18px")
              .add(Ui.img("blank")))
            .add(Q("td")
              .html("&nbsp;&nbsp;<b>" + _("Library") + "</b>"))
            .add(Q("td")
              .html("&nbsp;&nbsp;<b>" + _("Path") + "</b>"))
            .add(Q("td")
              .add(Ui.img("blank"))))
          .adds(paths.list.map(p -> pt.lib == p.lib
            ? Q("tr")
                .add(Q("td")
                  .add(Ui.img("blank")))
                .add(Q("td")
                  .style("text-align:center")
                  .add(Ui.link(e -> modifyCancel())
                    .add(Ui.img("cancel"))))
                .add(Q("td")
                  .style("text-align:center")
                  .add(Ui.link(e -> modifyConfirm(p))
                    .add(Ui.img("enter"))))
                .add(Q("td")
                  .add(libMd.value(p.lib)))
                .add(Q("td")
                  .klass("border")
                  .add(pathMd.value(p.path)))
                .add(Q("td")
                  .add(Ui.img(p.isValid ? "well" : "error")))
            : Q("tr")
                .add(Q("td")
                  .add(Ui.lightImg(p.isShown ? "out" : "in")))
                .add(Q("td")
                  .style("text-align:center")
                  .add(Ui.lightImg("edit")))
                .add(Q("td")
                  .style("text-align:center")
                  .add(Ui.lightImg("delete")))
                .add(Q("td")
                  .klass("border")
                  .text(Str.cutRight(p.lib, 20)))
                .add(Q("td")
                  .klass("border")
                  .text(Str.cutLeft(p.path, 60)))
                .add(Q("td")
                  .add(Ui.img(p.isValid ? "well" : "error")))
            )
          ))
        .add(Q("p")
          .style("text-align:center")
          .add(Ui.link(e -> changeLang())
            .klass("link")
            .html(_args(
              _("Change Language to %0"),
              [conf.lang == "en" ? "ES" : "EN"]
            )))
          .add(Q("span")
            .html("&nbsp;|&nbsp;"))
          .add(Ui.link(e -> changePass())
            .klass("link")
            .html(_("Change Password"))))
      ;
    case None:
      wg
        .removeAll()
        .add(Q("h2")
          .att("align", "center")
          .html(_("Libraries")))
        .add(Q("table")
          .att("border", 0)
          .att("align", "center")
          .klass("border")
          .style("background-color: rgb(255, 250, 250)")
          .add(Q("tr")
            .add(Q("td")
              .add(Ui.img("new")
                .style("vertical-align:-15%")))
            .add(Q("td")
              .att("align", "center")
              .att("colspan", 2)
              .add(Q("button")
                .style("width: 70px")
                .att("id", "newEnterBt")
                .on(CLICK, addPath)
                .add(Ui.img("enter").style("vertical-align:-10%"))))
            .add(Q("td")
              .add(libIn))
            .add(Q("td")
              .add(pathIn)))
          .add(Q("tr")
            .add(Q("td")
              .style("width:18px")
              .add(Ui.link(e -> changeShowAll())
                .add(Ui.img(conf.show ? "out" : "in"))))
            .add(Q("td")
              .style("width:18px")
              .add(Ui.img("blank")))
            .add(Q("td")
              .style("width:18px")
              .add(Ui.img("blank")))
            .add(Q("td")
              .html("&nbsp;&nbsp;<b>" + _("Library") + "</b>"))
            .add(Q("td")
              .html("&nbsp;&nbsp;<b>" + _("Path") + "</b>"))
            .add(Q("td")
              .add(Ui.img("blank"))))
          .adds(paths.list.length == 0
            ? [Q("tr")
              .add(Q("td")
                .att("colspan", 6)
                .att("align", "center")
                .klass("frame")
                .text(_("There is no library")))]
            : paths.list.map(p ->
               Q("tr")
                .add(Q("td")
                  .add(Ui.link(e -> setShow(p))
                    .add(Ui.img(p.isShown ? "out" : "in"))))
                .add(Q("td")
                  .style("text-align:center")
                  .add(Ui.link(e -> modifyShow(p))
                    .add(Ui.img("edit"))))
                .add(Q("td")
                  .style("text-align:center")
                  .add(Ui.link(e -> deletePath(p))
                    .add(Ui.img("delete"))))
                .add(Q("td")
                  .klass("border")
                  .text(Str.cutRight(p.lib, 20)))
                .add(Q("td")
                  .klass("border")
                  .text(Str.cutLeft(p.path, 60)))
                .add(Q("td")
                  .add(Ui.img(p.isValid ? "well" : "error")))
            )
          ))
        .add(Q("p")
          .style("text-align:center")
          .add(Ui.link(e -> changeLang())
            .klass("link")
            .html(_args(
              _("Change Language to %0"),
              [conf.lang == "en" ? "ES" : "EN"]
            )))
          .add(Q("span")
            .html("&nbsp;|&nbsp;"))
          .add(Ui.link(e -> changePass())
            .klass("link")
            .html(_("Change Password"))))
      ;
    }
  }

  // Control -------------------------------------------------------------------

  function update (): Void {
    Cts.client.ssend([
      "source" => Js.ws("Paths"),
      "rq" => Js.ws("update"),
      "conf" => conf.toJs(),
      "paths" => paths.toJs()
    ], rp -> {
      final paths = Paths.fromJs(rp["paths"]);
      mkMenu(paths);
      new PathsPg(wg, conf, paths, mkMenu);
    });
  }

  function changeShowAll (): Void {
    conf.show = !conf.show;
    update();
  }

  function changeLang (): Void {
    conf.lang = conf.lang == "es" ? "en" : "es";
    update();
  }

  function changePass (): Void {
    new ChangePass(wg, cm.Cts.app, () -> new PathsPg(wg, conf, paths, mkMenu));
  }

  function validate (lib: String, path: String, isNew: Bool): String {
    final plist = paths.list;

    if (lib == "") return _("Libray is missing.");
    if (path == "") return _("Path is missing.");
    if (isNew && It.from(plist).some(p -> p.lib == lib))
      return _args(_("Library '%0' is duplicated"), [lib]);

    final ix = Str.index(lib, "=@/&");
    if (ix != -1)
      return _args(_("Name '%0' contains '%1'"), [lib, lib.charAt(ix)]);

    if (lib.indexOf(" ") != -1)
      return _args(_("Name '%0' contains blanks"), [lib]);

    return "";
  }

  function addPath (): Void {
    final lib = libIn.getValue().trim();
    var path: String = pathIn.getValue().trim();

    while (path.endsWith("/")) {
      path = path.substring(0, path.length - 1);
    }
    path = path.trim();

    final error = validate(lib, path, true);
    if (error != "") {
      Ui.alert(error);
      return;
    }

    paths.list.push(new Lpath(lib, path, true, true));

    update();
  }

  function setShow (p): Void {
    if (p.isValid) p.isShown = !p.isShown;
    update();
  }

  function deletePath (p): Void {
    if (Ui.confirm(_args(_("Delete %0?"), [p.lib]))) {
      final ix = It.from(paths.list).indexf(p2 -> p2.lib == p.lib);
      if (ix != -1) {
        paths.list.splice(ix, 1);
        update();
      }
    }
  }

  function modifyShow (p): Void {
    modifyPath = Some(p);
    view();
  }

  function modifyCancel (): Void {
    modifyPath = None;
    view();
  }

  function modifyConfirm (p): Void{
    final lib = libMd.getValue().trim();
    var path: String = pathMd.getValue().trim();

    if (lib == p.lib && path == p.path) {
      modifyCancel();
      return;
    }

    final error = validate(lib, path, false);
    if (error != "") {
      Ui.alert(error);
      return;
    }

    p.lib = lib;
    p.path = path;
    update();
  }
}
