import * as iter from '../../_js/iter.js';import * as str from '../../_js/str.js';import * as bytes from '../../_js/bytes.js';import * as cryp from '../../_js/cryp.js';import * as dic from '../../_js/dic.js';import * as timer from '../../_js/timer.js';import * as js from '../../_js/js.js';import * as storage from '../../_js/storage.js';import * as sys from '../../_js/sys.js';import * as math from '../../_js/math.js';import * as domo from '../../_js/domo.js';import * as ui from '../../_js/ui.js';import * as arr from '../../_js/arr.js';import * as time from '../../_js/time.js';import * as client from '../../_js/client.js';import * as b64 from '../../_js/b64.js';




import * as menu from  "../../libdm/menu.js";
import * as accPg from  "../../pgs/settings/acc/accPg.js";
import * as calendarPg from  "../../pgs/settings/calendarPg/calendarPg.js";
import * as investorsPg from  "../../pgs/settings/investorsPg/investorsPg.js";
import * as nicks from  "../../pgs/settings/nicks/nicks.js";
import * as servers from  "../../pgs/settings/servers/servers.js";
import * as dmenu from  "../../wgs/dmenu.js";
import * as i18n from  "../../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);






export  function mk(wg, dbmenu, LcPath)  {sys.$params(arguments.length, 3);
  

  

  if (sys.asBool(!sys.asBool(LcPath))) arr.push(LcPath, "nicks");
  const target =sys.$checkNull(   
      sys.$eq(LcPath[0],"settings")|| sys.$eq(LcPath[0],"calendar")|| sys.$eq(LcPath[0],"servers")|| sys.$eq(LcPath[0],"annotations")|| sys.$eq(LcPath[0],"investors")?
        LcPath[0]:
      
        "nicks"
    );

  const Lopts =sys.$checkNull( [
    dmenu.mkHiddenButton(dbmenu),
    menu.separator2(),
    menu.tlink("nicks", II("Nicks"), ["settings"]),
    menu.separator(),
    menu.tlink("servers", II("Servers"), ["settings"]),
    menu.separator2(),
    menu.tlink("annotations", II("Annotations"), ["settings"]),
    menu.separator2(),
    menu.tlink("investors", II("Investors"), ["settings"])
  ]);

  const Ropts =sys.$checkNull( [
    menu.tlink("calendar", II("Calendar"), ["settings"])
  ]);

  dmenu.setDownMenu(dbmenu, menu.mk(Lopts, Ropts, target, false));

  switch (target) {
    case "servers":{
      servers.mk(wg, "");break;}
    case "annotations":{
      accPg.mk(wg);break;}
    case "investors":{ {
        const ix =sys.$checkNull(sys.asBool( sys.asBool(sys.$eq(arr.size(LcPath) , 2)) && sys.asBool(math.isDigits(LcPath[1])))
          ? math.fromStr(LcPath[1])[0]
          : 0)
        ;
        investorsPg.mk(wg, ix);
      }break;}
    case "calendar":{
      calendarPg.mk(wg);break;}
    default:{
      nicks.mk(wg);}
  }
};
