dmWeb/Dummy
===========

Template for making applications client-server.

Install in programming place (`dmWeb/app`)
----------------------------

1. Copy the directory Dummy to other place.
1. Remove the file `c/bin/Dummy`.
1. Update Dummy geany file, changing name of file, name of project.
   and command to launch browser.
1. Update `js/www/index-clean.html` and `js/www/index-closure.html` changing
   the line `<title>Dummy</title>`.
1. Delete the content of `js/www/js` direcoty but keeping itself.
1. Test dependencies, adding what are neccessary. Download them.
1. Adapt `c/Makefile` and `js/Make` to your needs, not forgeting to modify in
`c/Makefile` the line "`PRG = JsDoc`" with the name of the current application.

After that:

- In `c/src/main.c` change next lines:
```c
static char *app_name = "JsDoc";
static char *data_version = "201809";
static char *app_dir = "dmcgi/JsDoc";
static time_t expiration = 3600;
```
- In `js/src/Main.js` change next lines:
```javascript
const app = "JsDoc";
const version = "201809";
```

Install in web place (`www`)
--------------------------

- In `www/dmcgi/` create a symbolic link to `js/www` with the name of the 
  application.

Install in auxiliar web directory (`wwwcgi`)
--------------------------------------------

1. Compile the application
2. In `wwwcgi/bin` create a symbolic link to `c/bin/[program]`
3. In `wwwcgi/cgi.sh` add an entry like:
```bash
CTRL=$CTRL$(echo $RQ | grep -e '^Quotes:' | sed 's/Quotes:/Quotes /1')
```
In this point calling `localhost/dmcgi/[nameWeb]` the application should 
start.

