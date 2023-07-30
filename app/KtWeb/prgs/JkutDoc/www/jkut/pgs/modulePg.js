import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as i18n from  "../i18n.js";
import * as cts from  "../cts.js";
import * as msgPg from  "../pgs/msgPg.js";
import * as doc from  "../data/doc.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);

const tab =sys.$checkNull( "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");


export  async  function mk(wg, pack, path)  {sys.$params(arguments.length, 3);
  const Rp =sys.$checkNull( await  client.ssend({
    prg: "JkutDoc",
    source: "ModulePg",
    rq: "doc",
    pack: pack,
    path: path
  }));

  if (sys.asBool(!sys.asBool(Rp.doc))) {
    msgPg.mk(wg, i18n.fmt(II("[%0] Jkut file not found."), [path]), true);
    return;
  }

  const Doc =sys.$checkNull( doc.fromJs(Rp.doc[0]));

  arr.sort(Doc.Functions, function(E1, E2)  {sys.$params(arguments.length, 2);  return str.less(E1.name, E2.name);});
  arr.sort(Doc.Values, function(E1, E2)  {sys.$params(arguments.length, 2);  return str.less(E1.name, E2.name);});

  
   function index() {sys.$params(arguments.length, 0);
    
     function block(Entries, name)  {sys.$params(arguments.length, 2);
      if (sys.asBool(!sys.asBool(Entries)))  return [];

      const R =sys.$checkNull( []);
      arr.push(R, Q("tr")
        .add(Q("td")
          .att("colspan", "3")
          .html("<i>" + name + "</i>"))
      );
      const size =sys.$checkNull( arr.size(Entries));
      const h =sys.$checkNull( Math.floor((size - 1) / 3) + 1);
      for (let y = 0;y < h; ++y) {
        arr.push(R, Q("tr")
          .adds(arr.fromIter(iter.map(iter.$range(0,3), function(x)  {sys.$params(arguments.length, 1);
              const pos =sys.$checkNull( x * h + y);
              if (sys.asBool(pos < size)) {
                const E =sys.$checkNull( Entries[pos]);
                 return Q("td")
                  .add(Q("a")
                    .att("href", "#hp:" + E.name)
                    .html(tab + E.name))
                ;
              } else {
                 return Q("td");
              }
            })))
        );
      }
       return R;
    };

     return Q("div")
      .add(Q("p")
        .klass("frame2")
        .html("<b>" + path + "</b>"))
      .add(Q("table")
        .klass("main")
        .adds(block(Doc.Values, "Values"))
        .adds(block(Doc.Functions, "Functions")))
    ;
  };

  
   function overview()  {sys.$params(arguments.length, 0);
     return Q("div")
      .add(Q("p")
        .klass("frame")
        .html("<b>" + II("Overview") + "</b>"))
      .adds(mkHelp(Doc.doc))
      .add(Q("p")
        .html("<b>" + II("File") + "</b>")
        .add(Q("br"))
        .add(Q("a")
          .att("href", "?" + pack + "@" + path + "&hp:")
          .text(path + ".jkut")))
      .add(Q("hr"))
    ;
  };

  
   function body()  {sys.$params(arguments.length, 0);
    
     function block(Entries, name, isFunction)  {sys.$params(arguments.length, 3);  
      
       function endEntry(E)  {sys.$params(arguments.length, 1);
        const IsNewLine =sys.$checkNull( [true]);
        const Bf2 =sys.$checkNull( [""]);
        const code =sys.$checkNull( E.code);
        for (let i = 0;i < str.len(code); ++i) {
          const ch =sys.$checkNull( code[i]);
          if (sys.asBool(sys.asBool(IsNewLine[0]) && sys.asBool(sys.$neq(ch , "\n")))) {
            if (sys.asBool(ch <= " ")) {
              Bf2[0] +=sys.$checkExists(Bf2[0],sys.$checkNull( "&nbsp;"));
            } else {
              Bf2[0] +=sys.$checkExists(Bf2[0],sys.$checkNull( ch));
              IsNewLine[0] =sys.$checkExists(IsNewLine[0],sys.$checkNull( false));
            }
          } else if (sys.asBool(sys.$eq(ch , "\n"))) {
            Bf2[0] +=sys.$checkExists(Bf2[0],sys.$checkNull( "<br>"));
            IsNewLine[0] =sys.$checkExists(IsNewLine[0],sys.$checkNull( true));
          } else {
            Bf2[0] +=sys.$checkExists(Bf2[0],sys.$checkNull( ch));
          }
        }
         return Q("div")
          .add(Q("p")
            .html("<tt>" + Bf2[0] + "</tt>"))
          .adds(mkHelp(E.doc))
          .add(Q("hr"))
        ;
      };

       return arr.map(Entries, function(E)  {sys.$params(arguments.length, 1);
         return Q("div")
          .add(Q("h3")
            .att("id", "hp:" + E.name)
            .add(Q("span")
              .text(name + " "))
            .add(Q("a")
              .att(
                "href",
                "?" + pack + "@" + path + "&hp:" + E.link
              ).text(E.name)))
          .add(endEntry(E))
        ;}
      );
    };

     return Q("div")
      .adds(block(Doc.Values, "Value", false))
      .adds(block(Doc.Functions, "Function", true))
    ;
  };

  const barIx =sys.$checkNull( str.lastIndex(path, "/") + 1);
  Q("@title").text(cts.appName + " - " + sys.$slice(path,barIx,null));

  wg
    .removeAll()
    .add(index())
    .add(overview())
    .add(Q("hr").klass("frame"))
    .add(body())
    .adds(arr.fromIter(iter.map(iter.$range(0,30), function(i)  {sys.$params(arguments.length, 1);  return Q("p").html("&nbsp;");})))
  ;

  const lc =sys.$checkNull( window.location.href);
  const ix =sys.$checkNull( str.index(lc, "#"));
  if (sys.asBool(sys.$neq(ix ,  -1))) {
    const tg =sys.$checkNull( sys.$slice(lc,ix,null));
    if (sys.asBool(sys.$neq(tg , "#"))) {
      const E =sys.$checkNull( sys.$null((Q(tg).e)));
      if (sys.asBool(E)) {
        E[0].scrollIntoView(true);
      }
    }
  }

};


 function mkHelp(tx)  {sys.$params(arguments.length, 1);
  if (sys.asBool(!sys.asBool(str.trim(tx))))  return [];

  const html =sys.$checkNull( str.replace(str.replace(tx, "&", "&amp;"), "<", "&lt;"));
  const R =sys.$checkNull( []);
  arr.push(R, Q("table")
    .add(Q("tr")
      .add(Q("td")
        .klass("frame")
        .style("white-space: nowrap")
        .add(Q("pre")
          .style("font-size: 14px;")
          .html(html))))
  );
   return R;
};
