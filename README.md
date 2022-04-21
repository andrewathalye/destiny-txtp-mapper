# destiny-txtp-mapper
TXTP File Mapper and Exporter

This tool allows you to extract music from your own copy of the games Destiny and Destiny 2.  
The repository contains mappings for Destiny 2 Shadowkeep TXTP names and Destiny 1 Rise of Iron TXTP names, as well as a tool to export tracks to WAV,
identify tracks, and confirm them.

>	In order to comply with the Bungie EULA, you may only redistribute Destiny music files with their written permission. Therefore, please use this tool for your listening pleasure, and do not share the raw output files.

# How to use the mappings in this folder?
Follow the steps in the section "How can I get the necessary files?"
Next, collect the list of desired tracks and save it as tmp_tracks.txt.
An easy way to do this is: `cat tracks/tracks_*confirmed.txt > tmp_tracks.txt`
Run the tool as follows to export all confirmed tracks as WAVs:
`./src/mapper/mapper tmp_tracks.txt output`
If you would like some identified but unconfirmed tracks to also be exported, run: (-a allows unconfirmed export)
`./src/mapper/mapper -a tmp_tracks.txt output`

# How to create mappings?
Follow the steps in the section "How can I get the necessary files?"
First, you'll want to come up with a list of interesting bnk files to explore.
See the section "How can I find interesting banks?" for more information.
Place them in a format similar to the example file in a text file of your choice.
. means complete, + means needs identification, and ! means identified, but not confirmed.
Run the tool as follows with the -i flag to automatically help you identify txtps.
`./src/mapper/mapper -i [list file] [any name, will not be used here] 2>> tracks_unconfirmed.txt`
Note: Please do _not_ attempt to modify tracks_unconfirmed.txt while identifying. It can cause problems, unfortunately.

As you will note, this produces a file containing many ! lines.
The next step is to clean them up and organise them however you would like, and then
run the tool again in confirm mode:
`./src/mapper/mapper -c [list file] output 2>> tracks_confirmed.txt`

Once again, organise the file to your taste, and you will have a complete mapping file with your chosen tracks.

# How can I find interesting banks?
Find txtp entries, list their sizes, remove duplicates, and sort:
`./tools/search_sort.sh > matches/matches_sorted.txt`
Next, collect a master list of tracks: (This can be deleted after the next step)
`cat tracks/* > tmp_all.txt`
Next, remove entries that are already in one of your track files:
`./src/find/find tmp_all.txt < matches/matches_sorted.txt > matches/matches_new.txt`
To export the top 150 new banks by size for identification:
`./tools/extract_new.sh matches/matches_new.txt >> tracks/tracks_unidentified.txt`

# How can I get the necessary files?
In order to follow any of the steps in this document as written, you'll first need WSL x64 or 64-bit Linux.  
You will also need .NET SDK 6.0 and Python >= 3.0   
(These tools can be made to work on Windows using Windows-specific versions, however I have only tested them on Linux)

If you are just interested in music, visit https://github.com/andrewathalye/destiny-shadowkeep-music/ and read the instructions there.  
That repository contains a script that will automatically perform the below manual steps.  

Read on if you would like to use a different version than Destiny 2 Shadowkeep (not particularly straightforward, but supported in theory).   

# Manual Steps
Download the latest releases of these projects:
https://github.com/SteamRE/DepotDownloader
Use this to download the last release of Destiny 2 before 10 Nov 2020.

https://github.com/andrewathalye/destiny-unpacker-linux  
(Windows-specific version is https://github.com/nblockbuster/DestinyUnpackerCPP with different syntax)  
Use this to unpack the packages folder in the depot you downloaded. See README.md for more information.

https://github.com/bnnm/wwiser - download wwiser.pyz from releases
You may need TKinter to launch this, even though the instructions only use the CLI.

--

Run the following commands in a shell once you have unpacked the files :
`dotnet DepotDownloader.dll -app 1085660 -depot 1085661 -manifest 4160053308690659072 -username [steam username] `

The "packages folder" will be located somewhere within the DepotDownloader folder, alongside a Destiny 2.exe file  
To create the BNK and WEM files:
`./destinyunpacker prebl packages shadowkeep`  
You may see Oodle errors - these can be safely ignored.

Use wwiser to convert the bnk files to txtps:
`python3 wwiser.pyz -g shadowkeep/bnk/*.bnk`

`mv shadowkeep/wem shadowkeep/bnk/txtp`
`mv shadowkeep/bnk/txtp txtp-d2sk`

(Destiny 1 music extraction is beyond the scope of this README)  

In order to acquire the actual tools, please download the latest release or, alternatively,
see How to Complie the Necessary Tools below.  

If you intend to use Destiny 2 Shadowkeep mappings, enter `./tools/switch_d2sk.sh`  
(Otherwise, if you want to use Destiny 1 mappings and have extracted your own files, use `./tools/switch_d1.sh`)  

# How to Compile the Necessary Tools?
Run `./tools/build_vgmstream.sh` in order to build and install libvgmstream.so to the correct (local) directory.  
Please note that libvorbis should be installed.

In order to compile the tools here, you'll need GNAT or another Ada 2012 compiler, as well as gprbuild.
Please follow the below commands (in order) to compile all necessary tools:

`cd src/`
`gprbuild -Pmapper/mapper`
`gprbuild -Psearch/search`
`gprbuild -Pfind/find`

# Credits
Thank you to https://github.com/vgmstream/vgmstream, without which it would have been impossible to make this program.
