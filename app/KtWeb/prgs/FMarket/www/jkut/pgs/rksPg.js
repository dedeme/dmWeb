import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as menu from  "../libdm/menu.js";
import * as cts from  "../data/cts.js";
import * as flea from  "../data/flea.js";
import * as fns from  "../fns.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Url =sys.$checkNull( ui.url());
  const modelId =sys.$checkNull(sys.asBool( dic.hasKey(Url, "1")) ? Url["1"] : "");

  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "RksPg",
    rq: "idata",
    modelId:modelId
  }));

  const mdId =sys.$checkNull( Rp.mdId);
  const Models =sys.$checkNull( Rp.models);
  const Rks =sys.$checkNull( dic.fromArr(
    arr.map(Rp.rks, function(Kv)  {sys.$params(arguments.length, 1);  return [Kv[0], arr.map(Kv[1], flea.fromJs)];})
  ));
  const Dates =sys.$checkNull( dic.keys(Rks));
  arr.sort(Dates, function(d1, d2)  {sys.$params(arguments.length, 2);  return d1 > d2;});
  const page =sys.$checkNull( "rankings&" + mdId);

  const Lopts =sys.$checkNull( arr.reduce(
    Models, [],
    function(R, mId)  {sys.$params(arguments.length, 2);
      arr.push(R, menu.tlink("rankings&" + mId, mId, []));
      arr.push(R, menu.separator());
       return R;
    }
  ));
  arr.pop(Lopts);

  const menuWg =sys.$checkNull( menu.mk(Lopts, [], page, false));

  const size =sys.$checkNull( arr.size(Dates));
  const iV =sys.$checkNull( [0]);
  const Trs =sys.$checkNull( []); 
  while (sys.asBool(true)) {
    if (sys.asBool(sys.$eq(iV[0] , size))) break;
    const tr =sys.$checkNull( Q("tr"));
    for (let i = 0;i < 3; ++i) {
      if (sys.asBool(sys.$eq(iV[0] , size))) {
        for (let j = i;j < 3; ++j) tr.add(Q("td"));
        break;
      }
      const d =sys.$checkNull( Dates[iV[0]]);
      const Lst =sys.$checkNull( Rks[Dates[0]]);
      const Prv =sys.$checkNull(sys.asBool( sys.$eq(iV[0] , size - 1)) ? [] : Rks[Dates[iV[0]+1]]);
      const Nxt =sys.$checkNull(sys.asBool( sys.$eq(iV[0] , 0)) ? [] : Rks[Dates[iV[0]-1]]);
      tr.add(Q("td").add(mkRk(Lst, Prv, Nxt, d, Rks[d])));
      arr.push(Trs, tr);
      iV[0] +=sys.$checkExists(iV[0],sys.$checkNull( 1));
    }
  }

  const body =sys.$checkNull( Q("table")
    .att("align", "center")
    .adds(Trs))
  ;

  wg
    .removeAll()
    .add(menuWg)
    .add(body)
  ;

};








 function mkRk(Lst, Prv, Nxt, date, Rk)  {sys.$params(arguments.length, 5);
  const Rk0 =sys.$checkNull( Rk[0]);
  const npars =sys.$checkNull( arr.size(Rk0.params));
  const isDate =sys.$checkNull( arr.any(
    ["ME", "ME2", "MM", "MX_MN"],
    function(m)  {sys.$params(arguments.length, 1);  return sys.$eq(m , Rk0.model);}
  ));

  const Trs =sys.$checkNull( []); 
  for (let i = 0;i < arr.size(Rk); ++i) {
    const F =sys.$checkNull( Rk[i]);
    arr.push(
      Trs,
      Q("tr")
        .add(Q("td")
          .klass("fnumber")
          .add(mkSym(Prv, F.id, i)))
        .add(fns.mkTdN(F.id, 0)
            .style("" +
              mkLineThrough(Nxt, F.id) +
              mkBackground(Lst, F.id)
            )
          )
        .add(fns.mkTdN(F.cycle, 0))
        .adds(sys.asBool(sys.$eq(npars , 1))
            ? [fns.mkTdN(F.params[0], 4)]
            : [ fns.mkTdN(F.params[0],sys.asBool( isDate) ? 0 : 4),
                fns.mkTdN(F.params[1], 4)
              ]
          )
        .add(fns.mkTdN(F.points, 0))
    );
  }
   return Q("table")
    .klass("border")
    .add(Q("tr")
      .add(Q("td")
        .att("colspan",sys.asBool( sys.$eq(npars , 1)) ? 5 : 6)
        .klass("header")
        .text(time.toIso(time.fromStr(date)))))
    .add(Q("tr")
      .add(Q("td")
        .klass("header"))
      .add(Q("td")
        .klass("header")
        .text(II("Id.")))
      .add(Q("td")
        .klass("header")
        .text(II("Cy.")))
      .adds(sys.asBool(sys.$eq(npars , 1))
          ? [ Q("td")
                .klass("header")
                .text(II("P1"))
            ]
          : [ Q("td")
                .klass("header")
                .text(II("P1")),
              Q("td")
                .klass("header")
                .text(II("P2"))
            ]
        )
      .add(Q("td")
        .klass("header")
        .text(II("Points"))))
    .adds(Trs)
  ;

};



 function mkSym(Prv, id, i)  {sys.$params(arguments.length, 3);
  const ix =sys.$checkNull( arr.index(Prv, function(F)  {sys.$params(arguments.length, 1);  return sys.$eq(F.id , id);}));
  const d =sys.$checkNull( ix - i);
   return ui.img(sys.asBool(
    sys.$eq(ix ,  -1))
      ? "rk-new"
      :sys.asBool( d > 4)
        ? "rk-up2"
        :sys.asBool( d > 0)
          ? "rk-up"
          :sys.asBool( sys.$eq(d , 0))
            ? "rk-eq"
            :sys.asBool( d >=  -4)
              ? "rk-down"
              : "rk-down2"
  );
};



 function mkLineThrough(Next, id)  {sys.$params(arguments.length, 2);
  return sys.asBool( Next)
    ?sys.asBool( arr.any(Next, function(F)  {sys.$params(arguments.length, 1);  return sys.$eq(F.id , id);}))
      ? ""
      : "text-decoration: line-through;"
    : ""
  ;
};



 function mkBackground(Last, id)  {sys.$params(arguments.length, 2);
  return sys.asBool( sys.$eq(id , Last[0].id))
    ? "background-color: #a89247;"
    :sys.asBool( sys.$eq(id , Last[1].id))
      ? "background-color: #b8b8b8;"
      :sys.asBool( sys.$eq(id , Last[2].id))
        ? "background-color: #b7805b;"
        : ""
  ;
};
