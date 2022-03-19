# shadowkeep-txtp-mappings
Mappings from txtp names to human-readable track names for Shadowkeep.

As a result of Bungie's new stance on music redistribution, new methods have been developed for listening to sunsetted / hidden music.
This repository contains mappings between TXTP names for Shadowkeep and approximate track names, as well as a tool to produce named tracks from TXTP files.

Please note: In order to comply with the Bungie EULA, you may _not_ redistribute any music files unaltered (and redistributing modified files is also risky). Please only use this tool for your personal listening pleasure, and purchase the official soundtracks where available.

# How to use the mappings?
[TODO]

# How can I get the necessary files?
https://github.com/SteamRE/DepotDownloader
Use this to download the last release of Destiny 2 before 10 Nov 2020.
https://github.com/nblockbuster/DestinyUnpackerCPP
Use this excellent tool to extract wem's and bnk files from the pkgs:
`for i in packages/w64_audio*; do ./DestinyUnpackerCPP.exe -p [packages folder] -o shadowkeep -i "$i"; done`
DestinyUnpacker also supports a -f flag to unpack all pkgs (this did not work when I first tested it):
`./DestinyUnpackerCPP.exe -fp [packages folder] -o shadowkeep`
https://github.com/bnnm/wwiser
Use wwiser to convert the bnk files to txtps:
`wwiser -g *.bnk`
Finally, proceed to the section above titled: "How to use the mappings"
