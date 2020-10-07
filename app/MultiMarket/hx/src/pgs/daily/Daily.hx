// Copyright 17-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Store;
import dm.Opt;
import dm.Menu;
import wgs.Dmenu;
import data.Cts;
import data.DailyChart;
import I18n._;

/// Daily charts, main page.
class Daily {
  static final actKey = "MultiMarket_activity";
  static final ticKey = "MultiMarket_tic";
  static final ticIdKey = "MultiMarket_ticId";

  var wg: Domo;
  var dmenu: Dmenu;
  var lcPath: Array<String>;
  var server: String;
  var activity: String;
  var chartsData: Array<DailyChart>;
  var mSel: String;

  final serverWg = Q("span");
  final activityWg = Q("span");

  function new (
    wg:Domo, dmenu: Dmenu, lcPath: Array<String>,
    server: String, activity: String, chartsData: Array<DailyChart>
  ) {
    if (lcPath.length == 0) lcPath.push("summary");
    this.wg = wg;
    this.dmenu = dmenu;
    this.lcPath = lcPath;
    this.activity = activity;
    this.chartsData = chartsData;
    this.mSel = lcPath[0];

    serverWg.text(server);

    view();
    timer();
  }

  // View ----------------------------------------------------------------------

  function view () {
    function color (v: Float): String {
      return "color:" + (v > 0 ? "#00AAFF" : v < 0 ? "#FF8100" : "#404040");
    }

    var investing = 0.0;
    var assetsYesterday = 0.0;
    var assets = 0.0;
    for (d in chartsData) {
      for (e in d.managersData) {
        investing += e.stocks * e.price;
        assetsYesterday += e.stocks * d.close;
        assets += e.stocks * d.quotes[d.quotes.length - 1];
      }
    }
    final profits = assets - investing;
    final profitsToday = profits - assetsYesterday + investing;

    final wg = Q("div");
    final dmenu = dmenu;
    final mSel = mSel;

    final lopts = [
      dmenu.hiddingButton(),
      Menu.separator2(),
      Menu.tlink("summary", _("Summary"), "daily"),
      Menu.separator(),
      Menu.tlink("portfolio", _("Portfolio"), "daily"),
      Menu.separator(),
      Menu.tlink("all", _("All CO's"), "daily"),
      Menu.separator(),
      Menu.tlink("sel", _("Selection"), "daily")
    ];
    final ropts = [
      new MenuEntry(None, activityWg),
      Menu.separator2(),
      new MenuEntry(
        None,
        Q("span")
          .style(color(profitsToday))
          .text(Dec.toIso(profitsToday, 2))
      ),
      Menu.separator(),
      new MenuEntry(
        None,
        Q("span")
          .style(color(profits))
          .text(Dec.toIso(profits, 2))
      ),
      Menu.separator2(),
      new MenuEntry(None, serverWg),
      Menu.separator(),
      Menu.toption(">>", ">>", () -> newServer())
    ];
    dmenu.setDownMenu(new Menu(lopts, ropts, mSel));

    switch (mSel) {
      case "portfolio" | "all" | "sel": Cos.mk(wg, mSel, chartsData);
      default: new Summary(wg, chartsData);
    }

    this.wg
      .removeAll()
      .add(wg)
    ;
  }

  /**
      @private
      @param {string} msg
  **/
  function updateActivityWg (msg: String) {
    if (msg.length == 1) {
      final n = Std.parseInt(msg);
      msg = "";
      for (i in 0...n) {
        msg += " ·";
      }
    }
    activityWg
      .removeAll()
      .text(msg)
    ;
  }

  // Control -------------------------------------------------------------------

  function newServer () {
    serverWg
      .removeAll()
      .add(Ui.img("wait.gif")
        .style("vertical-align:middle"))
    ;
    Cts.client.send([
      "module" => Js.ws("daily"),
      "source" => Js.ws("daily"),
      "rq" => Js.ws("newServer")
    ], rp -> {});
    Daily.mk(wg, dmenu, lcPath);
  }

  function timer () {
    final tm = new haxe.Timer(15000);
    final tmId = Std.string(Date.now().getTime());
    Store.put(ticIdKey, tmId);

    function fn () {
      if (tmId != Opt.eget(Store.get(ticIdKey))) {
        tm.stop();
        return;
      }
      var previousAct = Opt.get(Store.get(actKey));
      if (previousAct == null) previousAct = "";
      final act = activity;
      final ticS = Opt.get(Store.get(ticKey));
      final tic = ticS == null ? 0 : Std.parseInt(ticS);

      if (act == _("Active")) {
        updateActivityWg(Std.string(4 - tic));
      } else {
        updateActivityWg(act);
      }

      if (tic < 4) {
        Store.put(ticKey, Std.string(tic + 1));
      } else {
        Store.put(ticKey, "0");
        if (tm != null) tm.stop();
        Daily.mk(wg, dmenu, lcPath);
      }
    }

    fn();
    tm.run = fn;
  }

  // Static --------------------------------------------------------------------

  /// Constructor
  ///   wg    : Container.
  ///   dmenu : Double menu.
  ///   lcPath: Location path.
  public static function mk (wg:Domo, dmenu: Dmenu, lcPath: Array<String>) {
    Cts.client.send([
      "module" => Js.ws("daily"),
      "source" => Js.ws("daily"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final chartsData = rp["chartsData"].ra().map(e -> DailyChart.fromJs(e));
      final server = rp["server"].rs();
      var activity = rp["activity"].rs();

      switch (activity) {
        case "Sleeping (1)": activity = _("Sleeping (1)");
        case "Historic": activity = _("Historic");
        case "Sleeping (2)": activity = _("Sleeping (2)");
        case "Activating": activity = _("Activating");
        case "Active": activity = _("Active");
        default: activity = _("Deactivating");
      }
      new Daily(wg, dmenu, lcPath, server, activity, chartsData);
    });
  }

}
