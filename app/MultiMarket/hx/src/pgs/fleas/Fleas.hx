// Copyright 09-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Ui;
import dm.Js;
import dm.It;
import dm.B64;
import dm.Menu;
import dm.Vmenu;
import data.Cts;
import wgs.Dmenu;
import wgs.Table; // include Col
import data.flea.Fmodel;
import data.flea.Eflea;
import data.flea.EfleaDate;
import data.flea.Frank;
import pgs.fleas.overview.Overview;
import pgs.fleas.ftests.Ftests;
import pgs.fleas.ranking.Ranking;
import pgs.fleas.charts.Charts;
import pgs.fleas.ranges.Ranges;
import pgs.fleas.ranges.RangesPlus;
import I18n._;

/// Fleas main page.
class Fleas {
  var wg: Domo;
  var dmenu: Dmenu;
  var lcPath: Array<String>;
  var models: Array<Fmodel>;
  var mSel: Fmodel;
  var target: String;

  // Constructor.
  //    wg    : Container.
  //    dmenu : Double menu
  //    lcPath: Location path
  //    models: Flea models.
  function new (
    wg: Domo, dmenu: Dmenu, lcPath: Array<String>, models: Array<Fmodel>
  ) {
    this.wg = wg;
    this.dmenu = dmenu;
    this.models = models;

    if (lcPath.length == 0) lcPath.push(models[0].id);
    switch (It.from(models).find(e -> e.id == lcPath[0])) {
      case None:
        mSel = models[0];
        lcPath.push(mSel.id);
      case Some(md):
        mSel = md;
    }

    if (lcPath.length == 1) lcPath.push("overview");
    this.lcPath = lcPath;

    switch (lcPath[1]) {
    case "overview" | "ranking" | "bests" | "pool" | "charts" |
      "ranges" | "ranges+" | "tests":
      target = lcPath[1];
    default:
      target = "overview";
    }

    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    final wg = Q("div");

    final ops: Array<VmenuEntry> = [];
    It.from(models).eachIx((e, ix) ->
      ops.push(Vmenu.option(
        e.id,
        e.id,
        () -> onModelSelection(ix)
      ))
    );
    ops.unshift(Vmenu.separator());
    ops.unshift(Vmenu.title(_("Models")));
    final vmenu = new Vmenu(ops, mSel.id);

    final link = "fleas&" + lcPath[0];
    final lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("overview", _("Overview"), link),
      Menu.separator2(),
      Menu.tlink("ranking", _("Ranking"), link),
      Menu.separator(),
      Menu.tlink("bests", _("Bests"), link),
      Menu.separator(),
      Menu.tlink("pool", _("Pool"), link),
      Menu.separator(),
      Menu.tlink("charts", _("Charts"), link),
      Menu.separator2(),
      Menu.tlink("ranges", _("Ranges"), link),
      Menu.separator(),
      Menu.tlink("ranges+", _("Ranges +"), link),
      Menu.separator2(),
      Menu.tlink("tests", _("Tests"), link)
    ];

    final ropts = [];
    dmenu.setDownMenu(new Menu(lopts, ropts, target));

    switch (target) {
    case "overview":
      new Overview(wg, mSel);
    case "ranking":
      ranking(wg);
    case "bests":
      bests(wg);
    case "pool":
      pool(wg);
    case "charts":
      charts(wg);
    case "ranges":
      Ranges.mk(wg, mSel);
    case "ranges+":
      RangesPlus.mk(wg, mSel, lcPath[0]);
    case "tests":
      new Ftests(wg, mSel);
    default:
      new Overview(wg, mSel);
    }

    this.wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;vertical-align:top")
            .add(vmenu.wg))
          .add(Q("td").html("&nbsp;&nbsp;&nbsp;&nbsp;"))
          .add(Q("td")
            .style("width:100%;vertical-align:top;")
            .add(wg))))
    ;
  }

  // Control -------------------------------------------------------------------

  function onModelSelection (id: Int) {
    mSel = models[id];
    js.Browser.location.assign("?fleas&" + mSel.id + "&" + lcPath[1]);
  }

  function ranking (wg:Domo) {
    wg
      .removeAll()
      .add(Q("div")
        .add(Ui.img("wait.gif")));

    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("fleas"),
      "rq" => Js.ws("ranking"),
      "modelId" => Js.ws(mSel.id)
    ], rp -> {
      final ranking = rp["ranking"].ra().map(e -> Frank.fromJs(e));
      final parNames = rp["parNames"].ra().map(e-> e.rs());
      final parDecs = rp["parDecs"].ra().map(e-> e.ri());

      final cols = mkHeaderBase();
      cols.insert(1, new Col(
        "", Col.ICON, -1, true, false
      ));

      for (i in 0...parNames.length) {
        cols.push(new Col(parNames[i], Col.PARAM, parDecs[i], false, false));
      }

      new Ranking(wg, cols, ranking, (e, i) ->
        (i == 2) ? chart(false, e) : chart(true, e)
      );
    });
  }

  function bests (wg: Domo) {
    wg
      .removeAll()
      .add(Q("div")
        .add(Ui.img("wait.gif")));

    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("fleas"),
      "rq" => Js.ws("bests"),
      "modelId" => Js.ws(mSel.id)
    ], rp -> {
      final bests = rp["bests"].ra().map(e -> EfleaDate.fromJs(e));
      final parNames = rp["parNames"].ra().map(e-> e.rs());
      final parDecs = rp["parDecs"].ra().map(e-> e.ri());

      final cols = mkHeaderBase();
      cols.insert(1, new Col(
        _("Date"), Col.DATE, -1, false, false
      ));

      for (i in 0...parNames.length) {
        cols.push(new Col(parNames[i], Col.PARAM, parDecs[i], false, false));
      }

      final table = bests.map(ed -> {
        final dt = ed.date; //DateDm.fromStr(ed.date).toString();
        final e = ed.eflea;
        final r: Array<Dynamic> = [
          e, 0, dt, e.flea.name, e.assets, e.profitsAvg, e.profitsVa,
          e.ev * 1000, e.buys, e.sells
        ];
        for (g in e.flea.params) r.push(g);
        return r;
      });
      new Table(wg, cols, table, 2, (e, i) -> chart(false, e));
    });
  }

  function pool (wg: Domo) {
    wg
      .removeAll()
      .add(Q("div")
        .add(Ui.img("wait.gif")));

    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("fleas"),
      "rq" => Js.ws("pool"),
      "modelId" => Js.ws(mSel.id)
    ], rp -> {
      final pool = rp["pool"].ra().map(e -> Eflea.fromJs(e));
      final parNames = rp["parNames"].ra().map(e-> e.rs());
      final parDecs = rp["parDecs"].ra().map(e-> e.ri());

      final cols = mkHeaderBase();

      for (i in 0...parNames.length) {
        cols.push(new Col(parNames[i], Col.PARAM, parDecs[i], false, false));
      }

      final table = pool.map(e -> {
        final r: Array<Dynamic> = [
          e, 0, e.flea.name, e.assets, e.profitsAvg, e.profitsVa,
          e.ev * 1000, e.buys, e.sells
        ];
        for (g in e.flea.params) r.push(g);
        return r;
      });
      new Table(wg, cols, table, 6, (e, i) -> chart(false, e));
    });
  }

  function charts (wg: Domo) {
    final modelId = lcPath[0];
    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("fleas"),
      "rq" => Js.ws("bests"),
      "modelId" => Js.ws(mSel.id)
    ], rp -> {
      final bests = rp["bests"].ra().map(e -> EfleaDate.fromJs(e));
      final parNames = rp["parNames"].ra().map(e-> e.rs());
      final parDecs = rp["parDecs"].ra().map(e-> e.ri());

      if (bests.length == 0) {
        wg
          .removeAll()
          .add(Q("table")
            .att("align", "center")
            .add(Q("tr")
              .add(Q("td")
                .klass("frame")
                .html(_("Without data")))));
        return;
      }

      var isAssets = true;
      var eflea = bests[0].eflea;

      final args = It.from(lcPath).drop(2).to();
      if (args.length == 2) {
        isAssets = Js.from(args[0]).rb();
        eflea = Eflea.fromJs(Js.from(B64.decode(args[1])));
      }

      new Charts(wg, modelId, isAssets, parNames, parDecs, eflea);
    });
  }

  // 'isSummary' is 'true' if chart is from assets summary.
  function chart (isSummary: Bool, eflea: Eflea): String {
    return '?fleas&${lcPath[0]}&charts' +
           '&${isSummary}&${B64.encode(eflea.toJs().to())}'
    ;
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg    : Container.
  ///   dmenu : Double menu
  ///   lcPath: Location path
  public static function mk (wg: Domo, dmenu: Dmenu, lcPath: Array<String>) {
    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("fleas"),
      "rq" => Js.ws("models")
    ], rp -> {
      final models = rp["models"].ra().map(e -> Fmodel.fromJs(e));
      new Fleas(wg, dmenu, lcPath, models);
    });
  }

  static function mkHeaderBase (): Array<Col> {
    return [
      new Col(_("Nº"), Col.COUNTER, 0, false, false),
      new Col(_("Id"), Col.STRING, -1, true, false),
      new Col(_("Assets"), Col.NUMBER, 2, false, false),
      new Col(_("Pf. Avg"), Col.NUMBER, 4, false, false),
      new Col(
        _("Pf. Var"), Col.NUMBER, 4,
        false, false
      ),
      new Col(
        _("Eval."), Col.NUMBER, 2,
        false, false
      ),
      new Col(
        _("Buys"), Col.NUMBER, 0,
        false, false
      ),
      new Col(
        _("Sells"), Col.NUMBER, 0,
        false, false
      )
    ];
  }
}
