#!/bin/bash

PRG="HxDoc"

export HAXE_STD_PATH=/dm/haxe/std
haxe build-test.hxml

echo '#!/bin/bash' > bin/$PRG
echo '/dm/hashlink/hl /dm/dmWeb/app/'$PRG'/server/bin/Main.hl $*' >> bin/$PRG
chmod +x bin/$PRG
