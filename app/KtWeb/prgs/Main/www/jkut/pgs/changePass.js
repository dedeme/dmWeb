import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as captcha from  "../libdm/captcha.js";
import * as cts from  "../data/cts.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);





export  function mk(wg, fn)  {sys.$params(arguments.length, 2);
  const app =sys.$checkNull( cts.appName);
  
   function mkCaptcha()  {sys.$params(arguments.length, 0);
     return captcha.mk(app + "__captcha", 3, "#f0f0f0", "#c0c0c0");};

  const oldPass =sys.$checkNull( ui.pass("newPass").att("id", "autofocus"));
  const newPass =sys.$checkNull( ui.pass("newPass2").att("id", "newPass"));
  const newPass2 =sys.$checkNull( ui.pass("acceptBt").att("id", "newPass2"));

  const Captcha =sys.$checkNull( [mkCaptcha()]);
  const Failed =sys.$checkNull( [false]);

  const Show =sys.$checkNull( [[]]);

  

  
   function cancel(ev)  {sys.$params(arguments.length, 1); fn();};

  
   async  function accept(ev)  {sys.$params(arguments.length, 1);
    const opass =sys.$checkNull( str.trim(oldPass.getValue()));
    const npass =sys.$checkNull( str.trim(newPass.getValue()));
    const npass2 =sys.$checkNull( str.trim(newPass2.getValue()));

    if (sys.asBool(sys.$eq(opass , ""))) {
    ui.alert(II("Current password is missing"));
      Show[0]();
      return;
    }
    if (sys.asBool(sys.$eq(npass , ""))) {
      ui.alert(II("New password is missing"));
      return;
    }
    if (sys.asBool(sys.$eq(npass2 , ""))) {
      ui.alert(II("Confirm password is missing"));
      return;
    }
    if (sys.asBool(sys.$neq(npass , npass2))) {
      ui.alert(II("New password and confirm password do not match"));
      return;
    }

    if (sys.asBool(sys.asBool(Captcha[0].isUpLimit()) && sys.asBool(!sys.asBool(Captcha[0].check())))) {
      ui.alert(II("Grey squares checks are wrong"));
      Captcha[0] =sys.$checkExists(Captcha[0],sys.$checkNull( mkCaptcha()));
      Show[0]();
      return;
    }

    const Rp =sys.$checkNull( await  client.ssend({
      prg: "Main",
      source: "ChangePass",
      rq: "changePass",
      user: client.userName(),
      old: client.crypPass(opass),
      "new": client.crypPass(npass)
    }));

    if (sys.asBool(Rp.ok)) {
      Captcha[0].reset();
      ui.alert(II("Password successfully changed"));
      fn();
    } else {
      Failed[0] =sys.$checkExists(Failed[0],sys.$checkNull( true));
      Captcha[0].increment();
      Captcha[0] =sys.$checkExists(Captcha[0],sys.$checkNull( mkCaptcha()));
      Show[0]();
    }
  };

  

  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    oldPass.value("");
    newPass.value("");
    newPass2.value("");

    const cancelBt =sys.$checkNull( Q("button")
      .on("click", cancel)
      .text(II("Cancel")))
    ;
    const acceptBt =sys.$checkNull( Q("button")
      .att("id", "acceptBt")
      .on("click", accept)
      .text(II("Accept")))
    ;

    const rows =sys.$checkNull( [
      Q("tr")
        .add(Q("td")
          .style("padding: 10px 0px 0px 10px;text-align:right;")
          .html(II("Current password")))
        .add(Q("td")
          .style("padding: 10px 10px 0px 10px;")
          .add(oldPass)),
      Q("tr")
        .add(Q("td")
          .style("padding: 5px 0px 0px 10px;text-align:right;")
          .html(II("New password")))
        .add(Q("td")
          .style("padding: 5px 10px 0px 10px;")
          .add(newPass)),
      Q("tr")
        .add(Q("td")
          .style("padding: 5px 0px 10px 10px;text-align:right;")
          .html(II("New password")))
        .add(Q("td")
          .style("padding: 5px 10px 10px 10px;")
          .add(newPass2)),
      Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .style(
            "border-top:1px solid #c9c9c9;" +
            "padding: 10px 10px 10px;text-align:right;")
          .add(Q("span")
            .add(cancelBt))
          .add(Q("span")
            .text("  "))
          .add(Q("span")
            .add(acceptBt)))
    ]);

    if (sys.asBool(Failed[0])) {
      rows.push(
        Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("border-top:1px solid #c9c9c9;" +
                   "adding: 10px 10px 10px;text-align:right;")
            .add(Q("table")
              .att("align", "center")
              .style(
                "background-color: rgb(250, 250, 250);" +
                "border: 1px solid rgb(110,130,150);" +
                "font-family: sans;font-size: 14px;" +
                "padding: 4px;border-radius: 4px;")
              .add(Q("tr")
                .add(Q("td")
                  .html(II("Wrong password"))))))
      );
    }

    if (sys.asBool(Captcha[0].isUpLimit())) {
      rows.push(
        Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .att("align", "center")
            .add(Captcha[0].wg))
      );
      rows.push(
        Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style("padding: 5px 0px 5px 10px;text-align:center;")
            .html(II("Check gray squares")))
      );
    }

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .html('&nbsp;<br>${app}<br>&nbsp;'))
      .add(Q("table")
        .att("align", "center")
        .style(
          "background-color: #f8f8f8;" +
          "border-collapse: collapse;" +
          "padding: 10px;" +
          "border: 1px solid rgb(110,130,150);")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .style(
              "background-color:#e8e8e8;" +
              "border-bottom:1px solid #c9c9c9;" +
              "padding: 10px;" +
              "color:#505050;"
            )
            .html("<big><big><b>" + II("Login") + "</big></big></b>")))
        .adds(rows));

    oldPass.e.focus();
  });

  Show[0]();
};
