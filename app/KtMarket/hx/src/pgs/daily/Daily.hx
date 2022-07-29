// Copyright 20-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.daily;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import dm.Store;
import dm.Opt;
import dm.Menu;
import wgs.Dmenu;
import data.DailyChart;
import data.IxsChartEntry;
import pgs.daily.Cos; // CosOrder
import pgs.daily.Invs; // InvsOrder
import I18n._;

/// Daily charts, main page.
class Daily {
  static final ticKey = Cts.appName + "_tic";

  final wg: Domo;
  final dmenu: Dmenu;
  final lcPath: Array<String>;
  final activity: String;
  final chartsData: Array<DailyChart>;
  final ixsData: Array<IxsChartEntry>;
  final capitals: Array<Float>;
  final mSel: String;
  public var order(default, null): CosOrder;
  public var iorder(default, null): InvsOrder;
  public var reverse(default, null): Bool;

  final serverWg = Q("span");
  final activityWg = Q("span");

  function new (
    wg:Domo, dmenu: Dmenu, lcPath: Array<String>,
    server: String, activity: String, chartsData: Array<DailyChart>,
    ixsData: Array<IxsChartEntry>, capitals: Array<Float>,
    order: CosOrder, iorder: InvsOrder, reverse: Bool
  ) {
    if (lcPath.length == 0) lcPath.push("summary");
    this.wg = wg;
    this.dmenu = dmenu;
    this.lcPath = lcPath;
    this.activity = activity;
    this.chartsData = chartsData;
    this.ixsData = ixsData;
    this.capitals = capitals;
    this.mSel = lcPath[0];
    this.order = order;
    this.iorder = iorder;
    this.reverse = reverse;

    serverWg.text(server);

    view();
    timer();
  }

  // View ----------------------------------------------------------------------

  function view () {
    function color (v: Float): String {
      return "color:" + (v > 0 ? "#00AAFF" : v < 0 ? "#FF8100" : "#404040");
    }
    function isReactivable (): Bool {
      return activity == _("Active") || Date.now().getHours() > 12;
    }

    var investing = 0.0;
    var assetsYesterday = 0.0;
    var assets = 0.0;
    for (d in chartsData) {
      for (e in d.investorsData) {
        investing += e.stocks * e.price;
        assetsYesterday += e.stocks * (e.todayBuy ? e.price : d.close);
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
      Menu.separator2(),
      Menu.tlink("portfolio", _("Portfolio"), "daily"),
      Menu.separator(),
      Menu.tlink("all", _("All CO's"), "daily"),
      Menu.separator(),
      Menu.tlink("sel", _("Selection"), "daily"),
      Menu.separator2()
    ];
    lopts.push(Menu.tlink("inv0", _("Inv") + "-0", "daily"));

    for (i in 1...chartsData[0].investorsData.length) {
      lopts.push(Menu.separator());
      lopts.push(Menu.tlink("inv" + i, _("Inv") + "-" + i, "daily"));
    }
    final ropts = [
      activity == _("Active")
        ? new MenuEntry(None, activityWg)
        : new MenuEntry(None, Q("span")),
      isReactivable() ?
        activity == _("Active")
          ? Menu.toption(activity, "[·]", reactivate)
          : Menu.toption(activity, activity, reactivate)
        : new MenuEntry(None, activityWg),
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
      Menu.toption(">>", ">>", newServer)
    ];
    dmenu.setDownMenu(new Menu(lopts, ropts, mSel));

    switch (mSel) {
      case "portfolio" | "all" | "sel": Cos.mk(wg, this, mSel, chartsData);
      default: if (mSel.startsWith("inv")) {
          final inv = Std.parseInt(mSel.substring(3));
          Invs.mk(wg, this, inv, chartsData);
        } else {
          new Summary(wg, chartsData, ixsData, capitals);
        }
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
      msg + " [·]";
    }
    activityWg
      .removeAll()
      .text(msg)
    ;
  }

  // Control -------------------------------------------------------------------

  public function setOrder (o: CosOrder): Void {
    order = o;
  }

  public function setIorder (o: InvsOrder): Void {
    iorder = o;
  }

  public function setReverse (isReverse: Bool): Void {
    reverse = isReverse;
  }

  function reactivate () {
    if (!Ui.confirm(_("Reactivate daily charts?"))) {
      return;
    }

    serverWg
      .removeAll()
      .add(Ui.img("wait.gif")
        .style("vertical-align:middle"))
    ;
    Cts.client.send([
      "module" => Js.ws("daily"),
      "source" => Js.ws("daily"),
      "rq" => Js.ws("reactivate")
    ], rp -> {
      Daily.mk(wg, dmenu, lcPath, order, iorder, reverse);
    });
  }

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
    Daily.mk(wg, dmenu, lcPath, order, iorder, reverse);
  }

  function timer () {
    final tm = new haxe.Timer(15000);

    function fn () {
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
        Daily.mk(wg, dmenu, lcPath, order, iorder, reverse);
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
  ///   order: Order to show charts.
  ///   reverse: 'true' if 'order' must be reversed.
  public static function mk (
    wg:Domo, dmenu: Dmenu, lcPath: Array<String>,
    order: CosOrder, iorder: InvsOrder, reverse: Bool
  ) {
    Cts.client.send([
      "module" => Js.ws("daily"),
      "source" => Js.ws("daily"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final chartsData = rp["chartsData"].ra().map(e -> DailyChart.fromJs(e));
      final ixsData = rp["indexesData"].ra().map(e -> IxsChartEntry.fromJs(e));
      final capitals = rp["capitals"].ra().map(e -> e.rf());
      final server = rp["server"].rs();
      var activity = rp["activity"].rs();

      switch (activity) {
        case "Sleeping": activity = _("Sleeping");
        case "Active": activity = _("Active");
        default: activity = _("Sleeping");
      }
      new Daily(
        wg, dmenu, lcPath, server, activity, chartsData, ixsData, capitals,
        order, iorder, reverse
      );
    });
  }

}
