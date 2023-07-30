import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as i18n from  "../i18n.js";
import * as cts from  "../data/cts.js";
import * as bkPaths from  "../data/bkPaths.js";
import * as runner from  "../pgs/runner.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);



export  async  function mk(wg)  {sys.$params(arguments.length, 1);
  const Rp =sys.$checkNull( await  client.send({
    prg: cts.appName,
    source: "BkListEditor",
    rq: "idata"
  }));
  const List =sys.$checkNull( arr.map(Rp.list, bkPaths.fromJs));

  const inId =sys.$checkNull( ui.field("inSource").att("id", "inId").style("width: 150px"));
  const inSource =sys.$checkNull( ui.field("inBackup").att("id", "inSource").style("width: 500px"));
  const inBackup =sys.$checkNull( ui.field("inId").att("id", "inBackup").style("width: 500px"));

  

  
   async  function add(ev)  {sys.$params(arguments.length, 1);
    const id =sys.$checkNull( inId.getValue());
    const From =sys.$checkNull( [inSource.getValue()]);
    const To =sys.$checkNull( [inBackup.getValue()]);
    while (sys.asBool(sys.asBool(str.len(From[0]) > 1) && sys.asBool(str.ends(From[0], "/")))) From[0] =sys.$checkExists(From[0],sys.$checkNull( sys.$slice(From[0],null, -1)));
    while (sys.asBool(sys.asBool(str.len(To[0]) > 1) && sys.asBool(str.ends(To[0], "/")))) To[0] =sys.$checkExists(To[0],sys.$checkNull( sys.$slice(To[0],null, -1)));
    const from =sys.$checkNull( From[0]);
    const to =sys.$checkNull( To[0]);

    const err =sys.$checkNull( [""]);
    if (sys.asBool(sys.$eq(id , ""))) err[0] =sys.$checkExists(err[0],sys.$checkNull( II("'Id' is missing")));
    else if (sys.asBool(sys.$eq(from , ""))) err[0] =sys.$checkExists(err[0],sys.$checkNull( II("'From' is missing")));
    else if (sys.asBool(sys.$eq(to , ""))) err[0] =sys.$checkExists(err[0],sys.$checkNull( II("'To' is missing")));
    else if (sys.asBool(sys.$eq(from , "/"))) err[0] =sys.$checkExists(err[0],sys.$checkNull( II("'From' is root")));
    else if (sys.asBool(sys.$eq(to , "/"))) err[0] =sys.$checkExists(err[0],sys.$checkNull( II("'To' is root")));
    if (sys.asBool(err[0])) {
      ui.alert(err[0]);
      return;
    }

    if (sys.asBool(arr.any(List, function(e)  {sys.$params(arguments.length, 1);  return sys.$eq(e.id , id);}))) {
      if (sys.asBool(!sys.asBool(ui.confirm(i18n.fmt(
        II("Already exists a backup with id '%0'\nOverwrite?"), [id]
      ))))) {
        return;
      }
    }

    await client.ssend({
      prg: cts.appName,
      source: "BkListEditor",
      rq: "addModify",
      id: id,
      from: from,
      to: to
    });
    await mk(wg);
    inId.e.focus();
  };

  
   function edit(bkPs)  {sys.$params(arguments.length, 1);
    inId.value(bkPs.id);
    inSource.value(bkPs.source);
    inBackup.value(bkPs.backup);
  };

  
   async  function del(bkPs)  {sys.$params(arguments.length, 1);
    const id =sys.$checkNull( bkPs.id);
    if (sys.asBool(!sys.asBool(ui.confirm(i18n.fmt(II("Delete '%0'?"), [id]))))) {
      return;
    }

    await client.ssend({
      prg: cts.appName,
      source: "BkListEditor",
      rq: "del",
      id: id
    });
    await mk(wg);
    inId.e.focus();
  };

  
   function go(bkPs)  {sys.$params(arguments.length, 1);
    runner.mk(wg, bkPs.id);
  };

  

  const tbEntry =sys.$checkNull( Q("table")
    .add(Q("tr")
      .add(Q("td")
        .att("colspan", 2))
      .add(Q("td")
        .style("text-align:left")
        .text(II("Id")))
      .add(Q("td"))
      .add(Q("td")
        .style("text-align:left")
        .text(II("Paths"))))
    .add(Q("tr")
      .add(Q("td")
        .att("colspan", 5)
        .add(Q("hr"))))
    .add(Q("tr")
      .add(Q("td")
        .style("width;5px")
        .add(ui.img("blank")))
      .add(Q("td")
        .style("width;5px")
        .add(ui.link(add)
          .klass("link")
          .add(ui.img("add"))))
      .add(Q("td")
        .add(inId))
      .add(Q("td")
        .style("text-align:right")
        .text(II("From")))
      .add(Q("td")
        .add(inSource)))
    .add(Q("tr")
      .add(Q("td")
        .att("colspan", 3))
      .add(Q("td")
        .style("text-align:right")
        .text(II("To")))
      .add(Q("td")
        .add(inBackup))))
    ;

  const trs =sys.$checkNull(sys.asBool( List)
    ? arr.map(List, function(E)  {sys.$params(arguments.length, 1);  return Q("tr")
        .add(Q("td")
          .klass("border")
          .add(Q("table")
            .add(Q("tr")
              .add(Q("td")
                .style("width;5px")
                .add(sys.asBool(E.state.length > 0)
                  ? ui.lightImg("edit")
                  : ui.link(function(ev)  {sys.$params(arguments.length, 1); edit(E);})
                    .klass("link")
                    .add(ui.img("edit"))
                  ))
              .add(Q("td")
                .style("width;5px")
                .add(sys.asBool(E.state.length > 0)
                  ? ui.lightImg("delete")
                  : ui.link(function(ev)  {sys.$params(arguments.length, 1); del(E);})
                    .klass("link")
                    .add(ui.img("delete"))
                  ))
              .add(Q("td")
                .add(sys.asBool(sys.asBool(sys.$neq(E.sourceError , "")) || sys.asBool(sys.$neq(E.backupError , "")))
                  ? Q("input").att("type", "text").disabled(true)
                      .style("width:150px;color:#800000")
                      .value(E.id)
                  : ui.link(function(ev)  {sys.$params(arguments.length, 1); go(E);})
                      .add(Q("input").att("type", "text")
                        .att("readonly", true)
                        .style("width:150px;color:#000000;cursor:pointer")
                        .value(E.id))
                  ))
              .add(Q("td")
                .style("text-align:right")
                .text(II("From")))
              .add(Q("td")
                .add(Q("input").att("type", "text").disabled(true)
                  .style("width:500px;text-align:right;color:#000000;")
                  .value(E.source)))
              .add(Q("td")
                .att("title", E.sourceError)
                .add(ui.img(sys.asBool(
                    sys.$neq(E.sourceError , ""))
                      ? "error"
                      : "ok"
                  )))
              .add(Q("td")
                .add(ui.img(sys.asBool(
                    sys.asBool(sys.$neq(E.sourceError , "")) || sys.asBool(sys.$neq(E.backupError , "")))
                      ? "error"
                      :sys.asBool( bkPaths.isRunning(E))
                        ? "wait.gif"
                        : "ok"
                  )))
                )
            .add(Q("tr")
              .add(Q("td")
                .att("colspan", 3))
              .add(Q("td")
                .style("text-align:right")
                .text(II("To")))
              .add(Q("td")
                .add(Q("input").att("type", "text").disabled(true)
                  .style("width:500px;text-align:right;color:#000000;")
                  .value(E.backup)))
              .add(Q("td")
                .att("title", E.backupError)
                .add(ui.img(sys.asBool(
                    sys.$neq(E.backupError , ""))
                      ? "error"
                      : "ok"
                  )))
              .add(Q("td")))))
      ;})
    : [
        Q("tr")
          .add(Q("td")
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td")
                  .klass("frame")
                  .text(II("Without Backups"))))))
      ])
  ;

  wg
    .removeAll()
    .add(Q("table")
      .style("border-collapse : collapse;")
      .att("align", "center")
      .add(Q("tr")
        .add(Q("td")
          .add(tbEntry)))
      .add(Q("tr")
        .add(Q("td")
          .add(Q("hr"))))
      .adds(trs))
  ;
  inId.e.focus();
};
