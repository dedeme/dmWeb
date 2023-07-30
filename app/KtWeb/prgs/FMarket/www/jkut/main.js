import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';




import * as menu from  "./libdm/menu.js";
import * as cts from  "./data/cts.js";
import * as msgPg from  "./pgs/msgPg.js";
import * as home from  "./pgs/home.js";
import * as fleasPg from  "./pgs/fleasPg.js";
import * as rksPg from  "./pgs/rksPg.js";
import * as summaryPg from  "./pgs/summaryPg.js";
import * as i18n from  "./i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


 async  function mk(wg)  {sys.$params(arguments.length, 1);
  const ok =sys.$checkNull( await  client.connect());
  if (sys.asBool(!sys.asBool(ok))) {
    ui.alert(II("KtWeb session is closed.\nAuthenticating from KtWeb:Main."));
    window.location.assign("http://" + window.location.host + "/Main");
    return;
  }

  const rp =sys.$checkNull( await  client.send({
    prg: "Main", 
    source: "Main",
    rq: "lang"
  }));
  if (sys.asBool(sys.$eq(rp.lang , "en"))) i18n.en();

  const Url =sys.$checkNull( ui.url());
  const page =sys.$checkNull(sys.asBool( dic.hasKey(Url, "0")) ? Url["0"] : "home");
  const menuWg =sys.$checkNull( menu.mk(
    [ menu.tlink("home", II("Home"), []),
      menu.separator(),
      menu.tlink("fleas", II("Fleas"), []),
      menu.separator(),
      menu.tlink("rankings", II("Rankings"), []),
      menu.separator(),
      menu.tlink("summary", II("Summary"), [])
    ],
    [],
    page,
    false
  ));

  const body =sys.$checkNull( Q("div"));

  wg
    .removeAll()
    .add(menuWg)
    .add(body)
  ;

  switch (page) {
    case "fleas":{ fleasPg.mk(body);break;}
    case "rankings":{ rksPg.mk(body);break;}
    case "summary":{ summaryPg.mk(body);break;}
    default:{ home.mk(body);}
  }
};



const wg =sys.$checkNull( Q("div"));


export  function load()  {sys.$params(arguments.length, 0);
  mk(wg);
};

client.init(true, "KtWeb", function()  {sys.$params(arguments.length, 0);
  const msgWg =sys.$checkNull( Q("div"));
  msgPg.mk(msgWg, II("Session is expired."), true);
  Q("@body")
    .removeAll()
    .add(msgWg)
    .add(cts.foot)
  ;
});

Q("@body")
  .removeAll()
  .add(wg)
  .add(cts.foot)
  .add(ui.upTop("up"))
;

load();
