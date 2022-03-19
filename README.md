# shadowkeep-txtp-mappings
Mappings from txtp names to human-readable track names for Shadowkeep.

As a result of Bungie's new stance on music redistribution, new methods have been developed for listening to sunsetted / hidden music.
This repository contains mappings between TXTP names for Shadowkeep and approximate track names, as well as a tool to produce named tracks from TXTP files.

Please note: In order to comply with the Bungie EULA, you may _not_ redistribute any music files unaltered (and redistributing modified files is also risky). Please only use this tool for your personal listening pleasure, and purchase the official soundtracks where available.

# How to use the mappings in this folder?
Follow the steps in the section "How can I get the necessary files?"
Run the tool as follows to export all confirmed tracks as WAVs: (-q suppresses warnings about unknown tracks)
`./mappingstool -q tracks.txt output`
If you would like some identified but unconfirmed tracks to also be exported, run: (-a allows unconfirmed export)
`./mappingstool -aq tracks.txt output`

# How to create mappings?
Follow the steps in the section "How can I get the necessary files?"
First, you'll want to come up with a list of interesting bnk files to explore.
Place them in a format similar to the example file in a text file of your choice.
. means complete, + means needs identification, and ! means identified, but not confirmed.
Run the tool as follows with the -i flag to automatically help you identify txtps.
`./mappingstool -i [list] [any name, will not be used here] 2> [output]`

As you will note, this produces a file on [output] containing many ! lines.
The next step is to clean them up and organise them however you would like, and then
run the tool again in confirm mode:
`./mappingstool -c [list] [any name, will not be used here] 2> confirmed_tracks.txt`

Once again, organise the file to your taste, and you will have a complete mapping file with your chosen tracks.

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

https://github.com/vgmstream/vgmstream
Compile vgmstream or download a precompiled version.
Note: to use the creation features, you'll need to compile vgmstream123 yourself.
Place vgmstream123 (if desired) and vgmstream-cli (rename if you only see test.exe) into this folder.

Finally, place the wem folder inside of the txtp folder and put the txtp folder in the same folder as this tool.
