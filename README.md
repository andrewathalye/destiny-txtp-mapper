# shadowkeep-txtp-mappings
TXTP File Mapper and Exporter

As a result of a recent incident involving mass takedowns of Destiny music, I've made a small tool to help you extract music from your own copy of the game.
This repository contains mappings for Destiny 2 Shadowkeep TXTP names, as well as a tool that allows you to export tracks to WAV format and identify them.

Please note: In order to comply with the Bungie EULA, you may only redistribute Destiny music files with their written permission. Therefore, please use this tool for your listening pleasure, and do not share the raw output files.

# How to use the mappings in this folder?
Follow the steps in the section "How can I get the necessary files?"
Next, collect the list of desired tracks and save it as tmp_tracks.txt.
An easy way to do this is: `cat tracks/tracks_*confirmed.txt > tmp_tracks.txt`
Run the tool as follows to export all confirmed tracks as WAVs:
`./tools-ada/mapper/mapper tmp_tracks.txt output`
If you would like some identified but unconfirmed tracks to also be exported, run: (-a allows unconfirmed export)
`./tools-ada/mapper/mapper -a tmp_tracks.txt output`

# How to create mappings?
Follow the steps in the section "How can I get the necessary files?"
First, you'll want to come up with a list of interesting bnk files to explore.
See the section "How can I find interesting banks?" for more information.
Place them in a format similar to the example file in a text file of your choice.
. means complete, + means needs identification, and ! means identified, but not confirmed.
Run the tool as follows with the -i flag to automatically help you identify txtps.
`./tools-ada/mapper/mapper -i [list file] [any name, will not be used here] 2>> tracks_unconfirmed.txt`
Note: Please do _not_ attempt to modify tracks_unconfirmed.txt while identifying. It can cause problems, unfortunately.

As you will note, this produces a file containing many ! lines.
The next step is to clean them up and organise them however you would like, and then
run the tool again in confirm mode:
`./tools-ada/mapper/mapper -c [list file] output 2>> tracks_confirmed.txt`

Once again, organise the file to your taste, and you will have a complete mapping file with your chosen tracks.

# How can I find interesting banks?
Find txtp entries, list their sizes, remove duplicates, and sort:
`./tools-sh/search_sort.sh > matches/matches_sorted.txt`
Next, collect a master list of tracks: (This can be deleted after the next step)
`cat tracks/* > tmp_all.txt`
Next, remove entries that are already in one of your track files:
`./tools-ada/find/find tmp_all.txt < matches/matches_sorted.txt > matches/matches_new.txt`
To export the top 150 new banks by size for identification:
`./tools-sh/extract_new.sh matches/matches_new.txt >> tracks/tracks_unidentified.txt`

# How can I get the necessary files?
In order to follow any of the steps in this document as written, you'll first need MinGW x64 and WSL x64 with GNAT installed

Run the following steps on Windows:

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

I ran the below steps on Linux. PulseAudio has a Windows version, so you may find luck using it with the tool, however I have yet to try it.

https://github.com/vgmstream/vgmstream
Put a dynamic-library build of vgmstream in src/vgmstream-ada/ext_lib/

Finally, place the wem folder inside of the txtp folder and put the txtp folder in the same folder as this tool.

Please see the below section if you need the tools in this repository (most people will, unless you plan to use foobar2000 or another player for the txtp files and not rename them).

# How to Compile the Necessary Tools?
In order to compile the tools here, you'll need GNAT or another Ada 2012 compiler, as well as gprbuild.
Please follow the below commands (in order) to compile all necessary tools:

`cd tools-ada/`
`gprbuild -Pmapper/mapper`
`gprbuild -Psearch/search`
`gprbuild -Pfind/find`
