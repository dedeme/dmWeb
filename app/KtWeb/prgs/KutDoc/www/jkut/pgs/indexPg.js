import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as i18n from  "../i18n.js";
import * as msgPg from  "../pgs/msgPg.js";
import * as cts from  "../cts.js";
import * as indexTree from  "../data/indexTree.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg, pack)  {sys.$params(arguments.length, 2);
  const Rp =sys.$checkNull( await  client.ssend({
    prg: "KutDoc",
    source: "IndexPg",
    rq: "index",
    pack: pack
  }));

  if (sys.asBool(!sys.asBool(Rp.index))) {
    msgPg.mk(wg, II("Library path not found o not valid."), true);
    return;
  }

  const Tree =sys.$checkNull( indexTree.fromJs(Rp.index[0]));
  const linkPrefix =sys.$checkNull( "?" + pack + "@");

  Q("@title").text(cts.appName + " - " + pack);

  const Trs =sys.$checkNull( []);
  add(Trs, Tree.Trees, "", 0, linkPrefix);

  wg
    .removeAll()
    .add(Q("div")
      .klass("frame")
      .add(Q("table")
        .klass("main")
        .adds(Trs)))
  ;
};


 function add(Trs, Trees, ppath, space, linkPrefix)  {sys.$params(arguments.length, 5);
  const path =sys.$checkNull(sys.asBool( sys.$neq(ppath , "")) ? ppath + "/" : ppath);

  arr.sort(Trees, function(T1, T2)  {sys.$params(arguments.length, 2);
    return sys.asBool( T1.Doc)
      ?sys.asBool( T2.Doc)
        ? str.less(str.toUpper(T1.id), str.toUpper(T2.id))
        : false
      :sys.asBool( T2.Doc)
        ? true
        : str.less(str.toUpper(T1.id), str.toUpper(T2.id))
    ;}
  );
  for (let T  of sys.$forObject( Trees)) {
    if (sys.asBool(T.Doc)) {
      arr.push(Trs, Q("tr")
        .add(Q("td")
          .style('width:10px;padding-left:' + space + 'px')
          .html(str.fmt(
              '<a href="%v%v%v">%v</a>',
              [linkPrefix, path, T.id, T.id]
            )))
        .add(Q("td")
          .style("padding-left:10px")
          .text(T.Doc[0]))
      );
    } else {
      arr.push(Trs, Q("tr")
        .add(Q("td")
          .style('padding-left:' + space + 'px')
          .html('<b>' + T.id + '</b>'))
        .add(Q("td"))
      );
      add(Trs, T.Trees, path + T.id, space + 20, linkPrefix);
    }
  }
};
