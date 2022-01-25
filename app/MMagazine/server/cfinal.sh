#!/bin/bash

PRG="MMagazine"

export HAXE_STD_PATH=/dm/haxe/std
haxe build-final.hxml
gcc -O3 -o bin/Main -I bin/_out bin/_out/Main.c -lhl;rm -R bin/_out

echo '#!/bin/bash' > bin/$PRG
echo '/dm/dmWeb/app/'$PRG'/server/bin/Main $*' >> bin/$PRG
chmod +x bin/$PRG
