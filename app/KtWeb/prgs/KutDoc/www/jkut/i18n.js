import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';



const esDic =sys.$checkNull( {
  "Click %0 to continue.": "Hacer click %0 para continuar.",
  "Delete %0?": "¿Eliminar %0?",
  "File": "Archivo",
  "Libraries": "Librerías",
  "Library path not found o not valid.": "La ruta de la librería no existe o no es válida.",
  "Name": "Nombre",
  "Name '%0' contains '%1'": "El nombre '%0' contiene '%1'",
  "Name '%0' contains blanks": "El nombre '%0' contiene espacios en blanco",
  "Name '%0' is repeated": "El nombre está repetido",
  "Name is missing": "Falta el nombre",
  "Overview": "Resumen",
  "Path": "Ruta",
  "Path '%0' does not start with '/'": "El path '%0' no comienza con '/'",
  "Path is '/'": "El path es '/'",
  "Path is missing": "Falta la ruta",
  "Session is expired.": "La sesión ha expirado.",
  "There are no libraries": "No hay librerías",
  "This source can not be selected, because it does not exist": "Esta librería no puede seleccionarse por que no existe",
  "here": "aquí"
});

export  function es() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "es"));};

const enDic =sys.$checkNull( {
  "Click %0 to continue.": "Click %0 to continue.",
  "Delete %0?": "Delete %0?",
  "File": "File",
  "Libraries": "Libraries",
  "Library path not found o not valid.": "Library path not found o not valid.",
  "Name": "Name",
  "Name '%0' contains '%1'": "Name '%0' contains '%1'",
  "Name '%0' contains blanks": "Name '%0' contains blanks",
  "Name '%0' is repeated": "Name '%0' is repeated",
  "Name is missing": "Name is missing",
  "Overview": "Overview",
  "Path": "Path",
  "Path '%0' does not start with '/'": "Path '%0' does not start with '/'",
  "Path is '/'": "Path is '/'",
  "Path is missing": "Path is missing",
  "Session is expired.": "Session is expired.",
  "There are no libraries": "There are no libraries",
  "This source can not be selected, because it does not exist": "This source can not be selected, because it does not exist",
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
