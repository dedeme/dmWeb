// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Definitions.

#ifndef DMC_DEFS_H
  #define DMC_DEFS_H

typedef struct js_Js Js;

///
#define REPEAT(n) { \
  int __i = (n) + 1; \
  while (--__i) {

///
#define _REPEAT }}

///
#define RANGE0(i, end) { \
  int __end = end; \
  int i = -1; \
  while (++i < __end) {

///
#define RANGE(i, begin, end) { \
  int __end = end; \
  int i = (begin) - 1; \
  while (++i < __end) {

///
#define _RANGE }}

///
#define IEACH(a, n) { \
  Iarr *__a = a; \
  int *__p = iarr_start(__a); \
  int *__pend = iarr_end(__a); \
  int n; \
  while(__p < __pend) { \
    n = *__p++;

///
#define IEACH_IX(a, n, ix) { \
  Iarr *__a = a; \
  int *__p = iarr_start(__a); \
  int *__pend = iarr_end(__a); \
  int n; \
  int ix = -1; \
  while(__p < __pend) { \
    ++ix; \
    n = *__p++;

///
#define DEACH(a, n) { \
  Darr *__a = a; \
  double *__p = darr_start(__a); \
  double *__pend = darr_end(__a); \
  double n; \
  while(__p < __pend) { \
    n = *__p++;

///
#define DEACH_IX(a, n, ix) { \
  Darr *__a = a; \
  double *__p = darr_start(__a); \
  double *__pend = darr_end(__a); \
  double n; \
  int ix = -1; \
  while(__p < __pend) { \
    ++ix; \
    n = *__p++;

///
#define EACH(a, type, e) { \
  Varr *__a = (Varr *)a; \
  void **__p = varr_start(__a); \
  void **__pend = varr_end(__a); \
  type *e; \
  while(__p < __pend) { \
    e = *__p++;

///
#define EACH_IX(a, type, e, i) { \
  Varr *__a = (Varr *)a; \
  void **__p = varr_start(__a); \
  void **__pend = varr_end(__a); \
  type *e; \
  int i = -1; \
  while(__p < __pend) { \
    ++i; \
    e = *__p++;

///
#define _EACH }}

///
#define FAIL(msg) sys_fail(msg, __FILE__, (char *)__func__, __LINE__);

/// CGI_GET read a 'field' of 'map'. If 'field' is not found produce a FAIL,
/// otherwise returns its value in 'type var' using 'f'.
/// <table><tr><td>
///   type   : type of var_new
///   var_new: name of new variable. It shuld be free.
///   fun    : function to pass 'Js' to 'type'
///   map    : A Map[Js]
///   field  : field key. Its value must be const char *
/// </table>
/// Examples:
/// <table><tr><td>
///   CGI_GET(int, i, js_ri, m, "index")
///   CGI_GET(Arr *, a, js_ra_new, m, "values")
/// </table>
#define CGI_GET(type, var_new, fun, map, field) \
  type var_new; \
  { \
    char *field2 = str_new(field); \
    Js *js = map_get_null(map, field2); \
    if (!js) { \
      char *msg = str_f_new("Field '%s' not found", field2); \
      sys_fail(msg, __FILE__, (char *)__func__, __LINE__); \
    } \
    var_new = fun(js); \
    free(field2); \
  }

/// Calls CGI_GET with 'var_new' as 'int'.
#define CGI_GET_BOOL(var_new, map, field) \
  CGI_GET(int, var_new, js_rb, map, field)

/// Calls CGI_GET with 'var_new' as 'int'.
#define CGI_GET_INT(var_new, map, field) \
  CGI_GET(int, var_new, js_ri, map, field)

/// Calls CGI_GET with 'var_new' as 'double'.
#define CGI_GET_DOUBLE(var_new, map, field) \
  CGI_GET(double, var_new, js_rd, map, field)

/// Calls CGI_GET with 'var_new' as 'char *'.
#define CGI_GET_STR(var_new, map, field) \
  CGI_GET(char *, var_new, js_rs_new, map, field)

/// Calls CGI_GET with 'var_new' as 'Arr[Js]'.
#define CGI_GET_ARR(var_new, map, field) \
  CGI_GET(char *, var_new, js_ra_new, map, field)

/// Calls CGI_GET with 'var_new' as 'Map[Js]'.
#define CGI_GET_MAP(var_new, map, field) \
  CGI_GET(char *, var_new, js_ro_new, map, field)

///
typedef void *(*FCOPY)(void *);

///
typedef int (*FGREATER)(void *, void *);

///
typedef void (*FPROC)(void *);

///
typedef int (*FPRED)(void *);

///
typedef Js *(*FTO)(void *);

///
typedef void *(*FFROM)(Js *);

#endif
