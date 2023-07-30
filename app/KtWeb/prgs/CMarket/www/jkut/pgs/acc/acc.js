import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as dmenu from  "../../wgs/dmenu.js";
import * as i18n from  "../../i18n.js";
import * as balance from  "../../pgs/acc/balance.js";
import * as tradingPg from  "../../pgs/acc/tradingPg.js";
import * as profitsPg from  "../../pgs/acc/profitsPg.js";
import * as companiesPg from  "../../pgs/acc/companiesPg.js";
import * as speedometers from  "../../pgs/acc/speedometers.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);






export  function mk(wg, dbMenu, LcPath)  {sys.$params(arguments.length, 3);
  const mSel =sys.$checkNull(sys.asBool( LcPath) ? LcPath[0] : "profits");

  

  

  const Lopts =sys.$checkNull( [
    dmenu.mkHiddenButton(dbMenu),
    menu.separator2(),
    menu.tlink("companies", II("Companies"), ["acc"]),
    menu.separator(),
    menu.tlink("balance", II("Balance"), ["acc"]),
    menu.separator(),
    menu.tlink("trading", II("Trading"), ["acc"]),
    menu.separator(),
    menu.tlink("profits", II("Profits"), ["acc"]),
    menu.separator2(),
    menu.tlink("speedometers", II("Speedometers"), ["acc"])
  ]);
  dmenu.setDownMenu(dbMenu, menu.mk(Lopts, [], mSel, false));

  switch (mSel) {
    case "companies":{ companiesPg.mk(wg);break;}
    case "balance":{ balance.mk(wg);break;}
    case "trading":{ tradingPg.mk(wg);break;}
    case "profits":{ profitsPg.mk(wg);break;}
    case "speedometers":{ speedometers.mk(wg);break;}
    default:{ profitsPg.mk(wg);} 
  }
};
