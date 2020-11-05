// Copyright 20-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import haxe.ds.Option;
import dm.Domo;
import dm.Ui.Q;
import dm.Ui;
import dm.Str;
import dm.It;
import dm.Js;
import dm.Menu;
import I18n._;
import I18n._args;
import I18n;
import data.Settings;
import data.Lpath;

/// Paths page.
class Paths {

  /// Constructor.
  ///   wg    : Container.
  ///   sett  : Settings data.
  ///   mkMenu: Function to make application menu.
  public static function mk (
    wg: Domo, sett: Settings, mkMenu: Settings -> Void
  ) {
    final libIn = Ui.field("pathIn").att("id", "autofocus").att("size", 20);
    final pathIn = Ui.field("newEnterBt").att("id", "pathIn").att("size", 60);
    final libMd = Ui.field("pathIn").att("id", "autofocus").att("size", 20);
    final pathMd = Ui.field("newEnterBt").att("id", "pathIn").att("size", 60);
    var view = () ->  {};


    var modifyPath: Option<Lpath> = None;

    // Control -----------------------------------------------------------------

    final update = () -> {
      Cts.client.ssend([
        "source" => Js.ws("Paths"),
        "rq" => Js.ws("update"),
        "data" => sett.toJs()
      ], rp -> {
        final sett = Settings.fromJs(rp["data"]);
        mkMenu(sett);
        mk(wg, sett, mkMenu);
      });
    }

    final changeShowAll = () -> {
      sett.show = !sett.show;
      update();
    }

    final changeLang = () -> {
      sett.lang = sett.lang == "es" ? "en" : "es";
      update();
    }

    final changePass = () -> {
      ChangePass.mk(wg, Cts.app, () -> mk(wg, sett, mkMenu));
    }

    final validate = (lib: String, path: String) -> {
      final paths = sett.paths;

      if (lib == "") return _("Source is missing.");
      if (path == "") return _("Path is missing.");
      if (It.from(paths).some(p -> p.lib == lib))
        return _args(_("Library '%0' is duplicated"), [lib]);

      final ix = Str.index(lib, "=@/&");
      if (ix != -1)
        return _args(_("Name '%0' contains '%1'"), [lib, lib.charAt(ix)]);

      if (lib.indexOf(" ") != -1)
        return _args(_("Name '%0' contains blanks"), [lib]);

      return "";
    }

    final addPath = () -> {
      final lib = libIn.getValue().trim();
      var path: String = pathIn.getValue().trim();

      while (path.endsWith("/")) {
        path = path.substring(0, path.length - 1);
      }
      path = path.trim();

      final error = validate(lib, path);
      if (error != "") {
        Ui.alert(error);
        return;
      }

      sett.paths.push(new Lpath(lib, path));

      update();
    }

    final setShow = p -> {
      p.isShown = !p.isShown;
      update();
    }

    final deletePath = p -> {
      if (Ui.confirm(_args(_("Delete %0?"), [p.lib]))) {
        final ix = It.from(sett.paths).indexf(p2 -> p2.lib == p.lib);
        if (ix != -1) {
          sett.paths.splice(ix, 1);
          update();
        }
      }
    }

    final modifyShow = p -> {
      modifyPath = Some(p);
      view();
    }

    final modifyCancel = () -> {
      modifyPath = None;
      view();
    }

    final modifyConfirm = p -> {
      final lib = libMd.getValue().trim();
      var path: String = pathMd.getValue().trim();

      var error: String;
      if (lib == p.lib) {
        if (path == p.path) {
          modifyCancel();
          return;
        }
        error = validate("", path);
      } else {
        error = validate(lib, path);
      }
      if (error != "") {
        Ui.alert(error);
        return;
      }

      p.lib = lib;
      p.path = path;
      update();
    }

    // View --------------------------------------------------------------------

    view = () -> {
      if (sett.lang == "es") I18n.es();
      else I18n.en();

      wg = switch (modifyPath) {
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
                  .on(CLICK, e -> addPath())
                  .add(Ui.img("enter").style("vertical-align:-10%"))))
              .add(Q("td")
                .add(libIn.disabled(true)))
              .add(Q("td")
                .add(pathIn.disabled(true))))
            .add(Q("tr")
              .add(Q("td")
                .style("width:18px")
                .add(Ui.lightImg(sett.show ? "out" : "in")))
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
            .adds(sett.paths.map(p -> pt.lib == p.lib
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
                [sett.lang == "en" ? "ES" : "EN"]
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
                  .on(CLICK, e -> addPath())
                  .add(Ui.img("enter").style("vertical-align:-10%"))))
              .add(Q("td")
                .add(libIn))
              .add(Q("td")
                .add(pathIn)))
            .add(Q("tr")
              .add(Q("td")
                .style("width:18px")
                .add(Ui.link(e -> changeShowAll())
                  .add(Ui.img(sett.show ? "out" : "in"))))
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
            .adds(sett.paths.length == 0
              ? [Q("tr")
                .add(Q("td")
                  .att("colspan", 6)
                  .att("align", "center")
                  .klass("frame")
                  .text(_("There is no library")))]
              : sett.paths.map(p ->
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
                [sett.lang == "en" ? "ES" : "EN"]
              )))
            .add(Q("span")
              .html("&nbsp;|&nbsp;"))
            .add(Ui.link(e -> changePass())
              .klass("link")
              .html(_("Change Password"))))
        ;
      }
    }

    view();
  }
}
