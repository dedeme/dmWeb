import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';



const esDic =sys.$checkNull( {
  "%0%1 not found.": "%0%1 no encontrado.",
  "Assets": "Activo",
  "Base": "Base",
  "Base Increment": "Incremento de la base",
  "Buys": "Compras",
  "Cash": "Caja",
  "Charts": "Gráficos",
  "Click %0 to continue.": "Hacer click %0 para continuar.",
  "Close": "Cierre",
  "Companies": "Compañías",
  "Current": "Actual",
  "Date": "Fecha",
  "Description": "Descripción",
  "Environment Increment": "Incremento del entorno",
  "Eval.": "Eval.",
  "H. Eval.": "H. Eval.",
  "H. Sales": "H. Ventas",
  "Historic": "Histórico",
  "Hot Map": "Mapa de calor",
  "Id": "Id",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "La sesión de KtWeb ha sido cerrada.\nHay que autenticarse en KtWeb:Main.",
  "Model": "Modelo",
  "Models": "Modelos",
  "Name": "Nombre",
  "Operations": "Operaciones",
  "Orders": "Órdenes",
  "Portfolio": "Cartera",
  "Prfs.": "Bfs.",
  "Profits (%)": "Beneficios (%)",
  "Quarantine": "Cuarentena",
  "Results": "Resultados",
  "Sales": "Ventas",
  "Sells": "Ventas",
  "Session is expired.": "La sesión ha expirado.",
  "Sls.": "Vts.",
  "Withdrawals": "Retiradas",
  "Without Data": "Sin datos",
  "here": "aquí"
});

export  function es() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "es"));};

const enDic =sys.$checkNull( {
  "%0%1 not found.": "%0%1 not found.",
  "Assets": "Assets",
  "Base": "Base",
  "Base Increment": "Base Increment",
  "Buys": "Buys",
  "Cash": "Cash",
  "Charts": "Charts",
  "Click %0 to continue.": "Click %0 to continue.",
  "Close": "Close",
  "Companies": "Companies",
  "Current": "Current",
  "Date": "Date",
  "Description": "Description",
  "Environment Increment": "Environment Increment",
  "Eval.": "Eval.",
  "H. Eval.": "H. Eval.",
  "H. Sales": "H. Sales",
  "Historic": "Historic",
  "Hot Map": "Hot Map",
  "Id": "Id",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "KtWeb session is closed.\nAuthenticating from KtWeb:Main.",
  "Model": "Model",
  "Models": "Models",
  "Name": "Name",
  "Operations": "Operations",
  "Orders": "Orders",
  "Portfolio": "Portfolio",
  "Prfs.": "Prfs.",
  "Profits (%)": "Profits (%)",
  "Quarantine": "Quarantine",
  "Results": "Results",
  "Sales": "Sales",
  "Sells": "Sells",
  "Session is expired.": "Session is expired.",
  "Sls.": "Sls.",
  "Withdrawals": "Withdrawals",
  "Without Data": "Without Data",
  "here": "here"
});

export  function en() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "en"));};


 function dicByKey(s)  {sys.$params(arguments.length, 1);    
  return sys.$eq(s,"es")? esDic:
  sys.$eq(s,"en")? enDic:
   "Unreachable"
;};

const Lang =sys.$checkNull( ["es"]);

export  function getLang() {sys.$params(arguments.length, 0);  return Lang[0];};


export  function tlt(s)  {sys.$params(arguments.length, 1);
  const T =sys.$checkNull( dic.get(dicByKey(Lang[0]), s));
  return sys.asBool( T) ? T[0] : s;
};


export  function fmt(tp, Rpls)  {sys.$params(arguments.length, 2);
  const R =sys.$checkNull( [tp]);
  for (let i = 0;i < arr.size(Rpls); ++i) R[0] =sys.$checkExists(R[0],sys.$checkNull( str.replace(R[0], "%" + sys.toStr(i), Rpls[i])));
   return R[0];
};
