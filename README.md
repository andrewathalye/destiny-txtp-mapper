# shadowkeep-txtp-mappings
Mappings from txtp names to human-readable track names for Shadowkeep.

As a result of a recent incident involving mass takedowns of Destiny music, I've made a small tool to help you extract music from your own copy of the game.
This repository contains mappings between TXTP names for Shadowkeep and approximate track names, as well as a tool to produce named tracks from TXTP files.

Please note: In order to comply with the Bungie EULA, you may _not_ redistribute any music files unaltered (and redistributing modified files is also risky). Please only use this tool for your personal listening pleasure, and purchase the official soundtracks where available.

# Important Note
The tools in this folder are currently being rewritten from C + Shell Scripts to 100% Ada in order to make them parallelised and faster.
As a result of this change, I will distribute GPL-licensed Linux GLIBC_x64 statically-linked binaries when the rewrite is complete.
Until the rewrite is complete, I recommend not using this tool as things may be in a state of flux.

# How to use the mappings in this folder?
Follow the steps in the section "How can I get the necessary files?"
Next, collect the list of desired tracks and save it as tracks.txt.
An easy way to do this is: `cat tracks/tracks_*confirmed.txt > tracks.txt`
Run the tool as follows to export all confirmed tracks as WAVs:
`./tools/mapper tracks.txt output`
If you would like some identified but unconfirmed tracks to also be exported, run: (-y allows unconfirmed export)
`./tools/mapper -y tracks.txt output`

# How to create mappings?
Follow the steps in the section "How can I get the necessary files?"
First, you'll want to come up with a list of interesting bnk files to explore.
See the section "How can I find interesting banks?" for more information.
Place them in a format similar to the example file in a text file of your choice.
. means complete, + means needs identification, and ! means identified, but not confirmed.
Run the tool as follows with the -i flag to automatically help you identify txtps.
`./tools/mapper -i [list file] [any name, will not be used here] 2>> tracks_unconfirmed.txt`

As you will note, this produces a file on [output] containing many ! lines.
The next step is to clean them up and organise them however you would like, and then
run the tool again in confirm mode:
`./tools/mapper -c [list file] [any name, will not be used here] 2>> tracks_confirmed.txt`

Once again, organise the file to your taste, and you will have a complete mapping file with your chosen tracks.

# How can I find interesting banks?
Use the tool "search.sh:" (Warning, this is extremely slow)
`./tools/search.sh | grep -vE '^0' > matches/matches_unsorted.txt`
This will produce an unsorted list of txtp lengths and the soundbank associated with the txtp.
Next, sort this list and clean out duplicate entries:
`./tools/sort.sh matches/matches_unsorted.txt | ./tools/cleaner | sort -rg | awk '{ print $2 " " $1}' > matches/matches_clean.txt`
Remove all entries that are already contained in a (master) tracks file:
`./tools/findnew.sh matches/matches_sorted.txt tracks.txt | sort -rg > matches/matches_new.txt`
To export the top 150 new banks by size for identification:
`./tools/extractnew.sh matches/matches_new.txt >> tracks/tracks_unidentified.txt`

# How can I get the necessary files?
In order to follow any of the steps in this document as written, you'll first need MinGW x64 or WSL x64 with GCC installed.
I personally ran the steps below up until the vgmstream compilation on Windows using MinGW, and then used Linux for the subsequent work.

https://github.com/SteamRE/DepotDownloader
Use this to download the last release of Destiny 2 before 10 Nov 2020.
`./DepotDownloader.exe -app 1085660 -depot 1085661 -manifest 4160053308690659072 -username [steam username] `

https://github.com/nblockbuster/DestinyUnpackerCPP
In case it is necessary when you read this, you can find the "oodle dll" in the Destiny 2 Shadowkeep bin folder as oo2_.....dll.

To create the BNK and WEM files:
`./DestinyUnpackerCPP.exe -fp [packages folder] -o shadowkeep`

https://github.com/bnnm/wwiser
Use wwiser to convert the bnk files to txtps:
`py wwiser.pyz -g *.bnk`

https://github.com/vgmstream/vgmstream
Compile vgmstream or download a precompiled version.
Note: to use the creation features, you'll need to compile vgmstream123 yourself. Don't forget to install libao, as it is a dep. for vgmstream123.
Place vgmstream123 (if desired) and vgmstream-cli (rename if you only see test.exe) into this folder.

Finally, place the wem folder inside of the txtp folder and put the txtp folder in the same folder as this tool.

To compile the tool itself:
First edit tools/mapper.c to set the VGMSTREAM paths to the correct ones for your system.
`cc tools/mapper.c -o tools/mapper`
To compile the cleaner tool (needed if you want to make your own mappings):
`cc tools/cleaner.c -o tools/cleaner`
