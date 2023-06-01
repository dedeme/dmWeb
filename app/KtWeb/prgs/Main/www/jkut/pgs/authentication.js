import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as captcha from  "../libdm/captcha.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);







export  function mk(wg, app, fnOk)  {sys.$params(arguments.length, 3);
  
   function mkCaptcha()  {sys.$params(arguments.length, 0);
     return captcha.mk(app + "__captcha", 3, "#f0f0f0", "#c0c0c0");};

  const Captcha =sys.$checkNull( [mkCaptcha()]);
  const Failed =sys.$checkNull( [false]);

  const Show =sys.$checkNull( [[]]);

  

  
   function changeLanguage(ev)  {sys.$params(arguments.length, 1);
    if (sys.asBool(sys.$eq(i18n.getLang() , "es"))) i18n.en();
    else i18n.es();
    Show[0]();
  };

  
   async  function send(user, pass, withExpiration)  {sys.$params(arguments.length, 3);
    if (sys.asBool(sys.$eq(user , ""))) {
      ui.alert(II("User name is missing"));
      return;
    }
    if (sys.asBool(sys.$eq(pass , ""))) {
      ui.alert(II("Password is missing"));
      return;
    }

    if (sys.asBool(sys.asBool(Captcha[0].isUpLimit()) && sys.asBool(!sys.asBool(Captcha[0].check())))) {
      ui.alert(II("Grey squares checks are wrong"));
      Captcha[0] =sys.$checkExists(Captcha[0],sys.$checkNull( mkCaptcha()));
      Show[0]();
      return;
    }

    const ok =sys.$checkNull( await  client.authentication(user, pass, withExpiration));
    if (sys.asBool(ok)) {
      Captcha[0].reset();
      fnOk();
    } else {
      Failed[0] =sys.$checkExists(Failed[0],sys.$checkNull( true));
      Captcha[0].increment();
      Captcha[0] =sys.$checkExists(Captcha[0],sys.$checkNull( mkCaptcha()));
      Show[0]();
    }
  };

  

  Show[0] =sys.$checkExists(Show[0], function()  {sys.$params(arguments.length, 0);
    const pass =sys.$checkNull( ui.pass("accept").att("id", "autofocus"));
    const userIn =sys.$checkNull( ui.field("autofocus").value("admin"));
    const persistent =sys.$checkNull( Q("input")
      .att("type", "checkbox")
      .style("vertical-align: middle")
      .checked(true))
    ;
    const accept =sys.$checkNull( Q("button")
      .att("id", "accept")
      .on("click", function(e)  {sys.$params(arguments.length, 1);
        send(
          str.trim(userIn.getValue()),
          str.trim(pass.getValue()),
          !sys.asBool(persistent.isChecked())
        );}
      )
      .text(II("Accept")))
    ;
    const rows =sys.$checkNull( [
      Q("tr")
        .add(Q("td")
          .style("padding: 10px 0px 0px 10px;text-align:right;")
          .html(II("User")))
        .add(Q("td")
          .style("padding: 10px 10px 0px 10px;")
          .add(userIn)),
      Q("tr")
        .add(Q("td")
          .style("padding: 10px 0px 0px 10px;text-align:right;")
          .html(II("Password")))
        .add(Q("td")
          .style("padding: 10px 10px 5px 10px;")
          .add(pass)),
      Q("tr")
        .add(Q("td")
          .att("colspan", 2)
          .style("border-top:1px solid #c9c9c9;" +
                 "padding: 5px 10px 10px;text-align:right;")
          .add(Q("table")
            .style(
              "border-collapse : collapse;" +
              "border : 0px;" +
              "width : 100%;")
            .add(Q("tr")
              .add(Q("td")
                .att("align", "center")
                .att("colspan", 2)
                .add(persistent)
                .add(Q("span")
                  .html("&nbsp;" + II("Keep connected")))))
            .add(Q("tr")
              .add(Q("td")
                .add(ui.link(changeLanguage)
                  .att("class", "link")
                  .html(sys.asBool(sys.$eq(i18n.getLang , "en")) ? "ES" : "EN")))
              .add(Q("td").att("align", "right").add(accept)))))
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
        .html("&nbsp;<br>" + app + "<br>&nbsp;"))
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

    pass.e.focus();
  });

  Show[0]();
};
