pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_stdint_intn_h;
with streamfile_h;
with sys_types_h;
with g72x_state_h;
with bits_stdint_uintn_h;
with stddef_h;
with System;
with streamtypes_h;
with Interfaces.C.Strings;

with VGMStream_Common; use VGMStream_Common;

package vgmstream_h is

  -- * vgmstream.h - definitions for VGMSTREAM, encapsulating a multi-channel, looped audio stream
  --  

  -- reasonable limits  
  -- 300 is Wwise min  
  -- found in some FSB5  
  -- +20000 isn't that uncommon  
  -- no ~5h vgm hopefully  
  -- Due mostly to licensing issues, Vorbis, MPEG, G.722.1, etc decoding is done by external libraries.
  -- * Libs are disabled by default, defined on compile-time for builds that support it  

  --#define VGM_USE_VORBIS
  --#define VGM_USE_MPEG
  --#define VGM_USE_G7221
  --#define VGM_USE_G719
  --#define VGM_USE_MP4V2
  --#define VGM_USE_FDKAAC
  --#define VGM_USE_MAIATRAC3PLUS
  --#define VGM_USE_FFMPEG
  --#define VGM_USE_ATRAC9
  --#define VGM_USE_CELT
  --#define VGM_USE_SPEEX
  -- The encoding type specifies the format the sound data itself takes  
  -- generates silence  
  -- PCM  
  -- little endian 16-bit PCM  
  -- big endian 16-bit PCM  
  -- 16-bit PCM with sample-level interleave (for blocks)  
  -- 8-bit PCM  
  -- 8-bit PCM with sample-level interleave (for blocks)  
  -- 8-bit PCM, unsigned (0x80 = 0)  
  -- 8-bit PCM, unsigned (0x80 = 0) with sample-level interleave (for blocks)  
  -- 8-bit PCM, sign bit (others are 2's complement)  
  -- 4-bit PCM, signed  
  -- 4-bit PCM, unsigned  
  -- 8-bit u-Law (non-linear PCM)  
  -- 8-bit u-Law (non-linear PCM) with sample-level interleave (for blocks)  
  -- 8-bit a-Law (non-linear PCM)  
  -- 32-bit float PCM  
  -- 24-bit PCM  
  -- ADPCM  
  -- CRI ADX  
  -- CRI ADX, encoding type 2 with fixed coefficients  
  -- CRI ADX, encoding type 4 with exponential scale  
  -- CRI ADX, type 8 encryption (God Hand)  
  -- CRI ADX, type 9 encryption (PSO2)  
  -- Nintendo DSP ADPCM  
  -- Nintendo DSP ADPCM with frame subinterframe  
  -- Nintendo DTK ADPCM (hardware disc), also called TRK or ADP  
  -- Nintendo AFC ADPCM  
  -- Silicon Graphics VADPCM  
  -- CCITT G.721  
  -- CD-ROM XA 4-bit  
  -- CD-ROM XA 8-bit  
  -- EA's Saturn XA (not to be confused with EA-XA)  
  -- Sony PS ADPCM (VAG)  
  -- Sony PS ADPCM with custom flag byte  
  -- Sony PS ADPCM with configurable frame size (int math)  
  -- Sony PS ADPCM with configurable frame size (float math)  
  -- Sony PSVita ADPCM  
  -- Electronic Arts EA-XA ADPCM v1 (stereo) aka "EA ADPCM"  
  -- Electronic Arts EA-XA ADPCM v1 (mono/interleave)  
  -- Electronic Arts EA-XA ADPCM v2  
  -- Maxis EA-XA ADPCM  
  -- Electronic Arts EA-XAS ADPCM v0  
  -- Electronic Arts EA-XAS ADPCM v1  
  -- IMA ADPCM (stereo or mono, low nibble first)  
  -- IMA ADPCM (mono/interleave, low nibble first)  
  -- DVI IMA ADPCM (stereo or mono, high nibble first)  
  -- DVI IMA ADPCM (mono/interleave, high nibble first)  
  -- 3DS IMA ADPCM  
  -- Heavy Iron Studios .snds IMA ADPCM  
  -- Gorilla Systems WV6 4-bit IMA ADPCM  
  -- High Voltage ALP 4-bit IMA ADPCM  
  -- Final Fantasy Tactics A2 4-bit IMA ADPCM  
  -- Blitz Games 4-bit IMA ADPCM  
  -- Microsoft IMA ADPCM  
  -- XBOX IMA ADPCM  
  -- XBOX IMA ADPCM (multichannel)  
  -- XBOX IMA ADPCM (mono/interleave)  
  -- IMA ADPCM w/ NDS layout  
  -- Eurocom 'DAT4' IMA ADPCM  
  -- Radical IMA ADPCM  
  -- Radical IMA ADPCM (mono/interleave)  
  -- Apple Quicktime IMA4  
  -- FMOD's FSB multichannel IMA ADPCM  
  -- Audiokinetic Wwise IMA ADPCM  
  -- Reflections IMA ADPCM  
  -- Rockstar AWC IMA ADPCM  
  -- Ubisoft IMA ADPCM  
  -- Ubisoft SCE IMA ADPCM  
  -- H4M IMA ADPCM (stereo or mono, high nibble first)  
  -- Capcom MT Framework IMA ADPCM  
  -- Crystal Dynamics IMA ADPCM  
  -- Microsoft ADPCM (stereo/mono)  
  -- Microsoft ADPCM (mono)  
  -- Microsoft ADPCM (Cricket Audio variation)  
  -- Westwood Studios VBR ADPCM  
  -- Yamaha AICA ADPCM (stereo)  
  -- Yamaha AICA ADPCM (mono/interleave)  
  -- Capcom's Yamaha ADPCM (stereo/mono)  
  -- Aska ADPCM  
  -- NXAP ADPCM  
  -- Tiger Game.com 4-bit ADPCM  
  -- Procyon Studio ADPCM  
  -- Level-5 0x555 ADPCM  
  -- lsf ADPCM (Fastlane Street Racing iPhone) 
  -- Konami MTAF ADPCM  
  -- Konami MTA2 ADPCM  
  -- Paradigm MC3 3-bit ADPCM  
  -- FMOD FADPCM 4-bit ADPCM  
  -- Argonaut ASF 4-bit ADPCM  
  -- Ocean DSA 4-bit ADPCM  
  -- Konami XMD 4-bit ADPCM  
  -- Tantalus 4-bit ADPCM  
  -- PC-FX 4-bit ADPCM  
  -- OKI 4-bit ADPCM with 16-bit output and modified expand  
  -- OKI 4-bit ADPCM with 16-bit output and cuadruple step  
  -- Platinum 4-bit ADPCM  
  -- LucasArts iMUSE Variable ADPCM  
  -- CompressWave Huffman ADPCM  
  -- others  
  -- SDX2 2:1 Squareroot-Delta-Exact compression DPCM  
  -- SDX2 2:1 Squareroot-Delta-Exact compression with sample-level interleave  
  -- CBD2 2:1 Cuberoot-Delta-Exact compression DPCM  
  -- CBD2 2:1 Cuberoot-Delta-Exact compression, with sample-level interleave  
  -- Activision EXAKT SASSC 8-bit DPCM  
  -- DERF 8-bit DPCM  
  -- WADY 8-bit DPCM  
  -- VisualArt's NWA DPCM  
  -- InterPlay ACM  
  -- Circus 8-bit ADPCM  
  -- Ubisoft 4/6-bit ADPCM  
  -- Electronic Arts MicroTalk (linear-predictive speech codec)  
  -- Circus VQ  
  -- Relic Codec (DCT-based)  
  -- CRI High Compression Audio (MDCT-based)  
  -- tri-Ace Codec (MDCT-based)  
  -- Xiph Vorbis with Ogg layer (MDCT-based)  
  -- Xiph Vorbis with custom layer (MDCT-based)  
  -- MPEG audio with custom features (MDCT-based)  
  -- EALayer3, custom MPEG frames  
  -- MP1 MPEG audio (MDCT-based)  
  -- MP2 MPEG audio (MDCT-based)  
  -- MP3 MPEG audio (MDCT-based)  
  -- ITU G.722.1 annex C (Polycom Siren 14)  
  -- ITU G.719 annex B (Polycom Siren 22)  
  -- AAC (MDCT-based)  
  -- Sony ATRAC3plus (MDCT-based)  
  -- Sony ATRAC9 (MDCT-based)  
  -- Custom Xiph CELT (MDCT-based)  
  -- Custom Speex (CELP-based)  
  -- Formats handled by FFmpeg (ATRAC3, XMA, AC3, etc)  
   type coding_t is 
     (coding_SILENCE,
      coding_PCM16LE,
      coding_PCM16BE,
      coding_PCM16_int,
      coding_PCM8,
      coding_PCM8_int,
      coding_PCM8_U,
      coding_PCM8_U_int,
      coding_PCM8_SB,
      coding_PCM4,
      coding_PCM4_U,
      coding_ULAW,
      coding_ULAW_int,
      coding_ALAW,
      coding_PCMFLOAT,
      coding_PCM24LE,
      coding_CRI_ADX,
      coding_CRI_ADX_fixed,
      coding_CRI_ADX_exp,
      coding_CRI_ADX_enc_8,
      coding_CRI_ADX_enc_9,
      coding_NGC_DSP,
      coding_NGC_DSP_subint,
      coding_NGC_DTK,
      coding_NGC_AFC,
      coding_VADPCM,
      coding_G721,
      coding_XA,
      coding_XA8,
      coding_XA_EA,
      coding_PSX,
      coding_PSX_badflags,
      coding_PSX_cfg,
      coding_PSX_pivotal,
      coding_HEVAG,
      coding_EA_XA,
      coding_EA_XA_int,
      coding_EA_XA_V2,
      coding_MAXIS_XA,
      coding_EA_XAS_V0,
      coding_EA_XAS_V1,
      coding_IMA,
      coding_IMA_int,
      coding_DVI_IMA,
      coding_DVI_IMA_int,
      coding_3DS_IMA,
      coding_SNDS_IMA,
      coding_QD_IMA,
      coding_WV6_IMA,
      coding_ALP_IMA,
      coding_FFTA2_IMA,
      coding_BLITZ_IMA,
      coding_MS_IMA,
      coding_XBOX_IMA,
      coding_XBOX_IMA_mch,
      coding_XBOX_IMA_int,
      coding_NDS_IMA,
      coding_DAT4_IMA,
      coding_RAD_IMA,
      coding_RAD_IMA_mono,
      coding_APPLE_IMA4,
      coding_FSB_IMA,
      coding_WWISE_IMA,
      coding_REF_IMA,
      coding_AWC_IMA,
      coding_UBI_IMA,
      coding_UBI_SCE_IMA,
      coding_H4M_IMA,
      coding_MTF_IMA,
      coding_CD_IMA,
      coding_MSADPCM,
      coding_MSADPCM_int,
      coding_MSADPCM_ck,
      coding_WS,
      coding_AICA,
      coding_AICA_int,
      coding_CP_YM,
      coding_ASKA,
      coding_NXAP,
      coding_TGC,
      coding_NDS_PROCYON,
      coding_L5_555,
      coding_LSF,
      coding_MTAF,
      coding_MTA2,
      coding_MC3,
      coding_FADPCM,
      coding_ASF,
      coding_DSA,
      coding_XMD,
      coding_TANTALUS,
      coding_PCFX,
      coding_OKI16,
      coding_OKI4S,
      coding_PTADPCM,
      coding_IMUSE,
      coding_COMPRESSWAVE,
      coding_SDX2,
      coding_SDX2_int,
      coding_CBD2,
      coding_CBD2_int,
      coding_SASSC,
      coding_DERF,
      coding_WADY,
      coding_NWA,
      coding_ACM,
      coding_CIRCUS_ADPCM,
      coding_UBI_ADPCM,
      coding_EA_MT,
      coding_CIRCUS_VQ,
      coding_RELIC,
      coding_CRI_HCA,
      coding_TAC)
   with Convention => C;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:229

  -- The layout type specifies how the sound data is laid out in the file  
  -- generic  
  -- straight data  
  -- interleave  
  -- equal interleave throughout the stream  
  -- headered blocks  
  -- GTA IV .ivaud blocks  
  -- DefJam Rapstar .tra blocks  
  -- newest Electronic Arts blocks, found in SNS/SNU/SPS/etc formats  
  -- Rockstar AWC  
  -- Guitar Hero II (PS2)  
  -- XVAG subsongs [God of War III (PS4)]  
  -- EA WVE au00 blocks  
  -- EA WVE Ad10 blocks  
  -- Dream Factory STHD  
  -- H4M video  
  -- XA in AIFF files [Crusader: No Remorse (SAT), Road Rash (3DO)]  
  -- otherwise odd  
  -- song divided in segments (song sections)  
  -- song divided in layers (song channels)  
   type layout_t is 
     (layout_none,
      layout_interleave,
      layout_blocked_ast,
      layout_blocked_halpst,
      layout_blocked_xa,
      layout_blocked_ea_schl,
      layout_blocked_ea_1snh,
      layout_blocked_caf,
      layout_blocked_wsi,
      layout_blocked_str_snds,
      layout_blocked_ws_aud,
      layout_blocked_matx,
      layout_blocked_dec,
      layout_blocked_xvas,
      layout_blocked_vs,
      layout_blocked_mul,
      layout_blocked_gsb,
      layout_blocked_thp,
      layout_blocked_filp,
      layout_blocked_ea_swvr,
      layout_blocked_adm,
      layout_blocked_bdsp,
      layout_blocked_mxch,
      layout_blocked_ivaud,
      layout_blocked_tra,
      layout_blocked_ps2_iab,
      layout_blocked_vs_str,
      layout_blocked_rws,
      layout_blocked_hwas,
      layout_blocked_ea_sns,
      layout_blocked_awc,
      layout_blocked_vgs,
      layout_blocked_xwav,
      layout_blocked_xvag_subsong,
      layout_blocked_ea_wve_au00,
      layout_blocked_ea_wve_ad10,
      layout_blocked_sthd,
      layout_blocked_h4m,
      layout_blocked_xa_aiff,
      layout_blocked_vs_square,
      layout_blocked_vid1,
      layout_blocked_ubi_sce,
      layout_segmented,
      layout_layered)
   with Convention => C;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:285

  -- The meta type specifies how we know what we know about the file.
  -- * We may know because of a header we read, some of it may have been guessed from filenames, etc.  

  -- Nintendo standard GC ADPCM (DSP) header  
  -- Star Fox Assault "Cstr"  
  -- Retro: Metroid Prime 2 "RS03"  
  -- Paper Mario 2 STM  
  -- Retro: Metroid Prime 2 title  
  -- Retro: Metroid Prime 3 (Wii), Donkey Kong Country Returns (Wii)  
  -- Retro: Donkey Kong Country Tropical Freeze (Wii U)  
  -- Monopoly Party single header stereo  
  -- Bomberman Jetters .dsp  
  -- Free Radical GC games  
  -- some of Traveller's Tales games  
  -- Conan .str files  
  -- .sad  
  -- .wsi  
  -- Traveller's Tales games  
  -- .mus  
  -- Phantom Brave (WII)  
  -- Vertigo (Wii)  
  -- Konami: Yu-Gi-Oh! The Falsebound Kingdom (NGC), Hikaru no Go 3 (NGC)  
  -- Nintendo STRM  
  -- Nintendo RSTM (Revolution Stream, similar to STRM)  
  -- AFC  
  -- AST  
  -- single-stream RWSD  
  -- single-stream RWAR  
  -- contents of RWAR  
  -- contents of CWAR  
  -- contents of FWAR  
  -- RSTM with 44->22khz hack  
  -- THP movie files  
  -- Atlus' mutant shortened RSTM  
  -- Ridge Racer DS  
  -- Wii BNS Banner Sound (similar to RSTM)  
  -- Wii U Boot Sound  
  -- CRI ADX "type 03"  
  -- CRI ADX "type 04"  
  -- CRI ADX "type 05"  
  -- CRI AIX  
  -- CRI AAX  
  -- CRI ADPCM_WII, like AAX with DSP  
  -- HAL Labs HALPST  
  -- GCSW (PCM)  
  -- tri-Crescendo CAF  
  -- U-Sing .myspd  
  -- Her Ineractive .his  
  -- Bandai Namco Sound Format  
  -- CD-ROM XA  
  -- headerless PS-ADPCM  
  -- KOEI MIC File  
  -- VAGi Interleaved File  
  -- VAGp Mono File  
  -- VAGp with Little Endian Header  
  -- Acclaim Austin Audio VAG header  
  -- Blitz Games STR+WAV files  
  -- PsychoNauts Bgm File  
  -- VPK Audio File  
  -- Beatmania thing  
  -- Langrisser 3 IVB  
  -- some Might & Magics SSND header  
  -- Square SVS  
  -- Dino Crisis 3  
  -- Test Drive Unlimited  
  -- Knights of the Temple 2  
  -- Various Capcom games  
  -- RenderWare games (only when using RW Audio middleware)  
  -- FMOD Sample Bank, version 1  
  -- FMOD Sample Bank, version 2  
  -- FMOD Sample Bank, version 3.0/3.1  
  -- FMOD Sample Bank, version 4  
  -- FMOD Sample Bank, version 5  
  -- Air Force Delta Storm (XBOX)  
  -- Microsoft XACT framework (Xbox, X360, Windows)  
  -- Driver - Parallel Lines (PS2)  
  -- Krome PS2 games  
  -- Legaia 2 [no header_id]  
  -- Resident Evil - Dead Aim  
  -- Ibara, Mushihimesama  
  -- Midnight Club 3  
  -- Dance Dance Revolution  
  -- Tokobot Plus - Myteries of the Karakuri  
  -- Lunar - Eternal Blue  
  -- Konami KCEJ East: Ephemeral Fantasia, Yu-Gi-Oh! The Duelists of the Roses, 7 Blades  
  -- Legacy of Kain - Blood Omen 2 (PS2)  
  -- Pro Baseball Spirits 5  
  -- TECMO badflagged stream  
  -- Enthusia  
  -- Baldur's Gate - Dark Alliance  
  -- Ty - The Tasmanian Tiger  
  -- Capcom DSP Header [no header_id]  
  -- SEGA Stream Asset Builder  
  -- variant of SEGA Stream Asset Builder  
  -- Bio Hazard 2  
  -- Eldorado Gate  
  -- Geometry Wars - Galaxies  
  -- PS2 Int file with Header  
  -- XG3 Extreme-G Racing  
  -- Mario Strikers Charged (Wii)  
  -- Defencer (GC)  
  -- Various (SPT+SPT DSP)  
  -- Various (ISH+ISD DSP)  
  -- Tecmo games (Super Swing Golf 1 & 2, Quamtum Theory)  
  -- WWE Day of Reckoning  
  -- Final Fantasy: Crystal Chronicles  
  -- Beyond Good & Evil, Rayman Raving Rabbids  
  -- Metal Slug Anthology  
  -- Golden Gashbell Full Power  
  -- Wall-E / Pixar games  
  -- WWE WrestleMania X8  
  -- Tokyo Xtreme Racer DRIFT 2  
  -- Jackie Chan - Stuntmaster  
  -- Merged MIH+MIB  
  -- Mario Party 6  
  -- Miss Moonligh  
  -- Guilty Gear X  
  -- ASS  
  -- Eragon  
  -- Final Fantasy Tactics A2  
  -- Zack and Wiki  
  -- Guitar Hero Encore - Rocks the 80s  
  -- Excite Trucks  
  -- Crypt Killer  
  -- Tiny Toon Adventures: Defenders of the Universe  
  -- Ape Escape 2  
  -- RC Revenge Pro  
  -- Konami DSP header, found in various games  
  -- Ubisoft CKD RIFF header (Rayman Origins Wii)  
  -- XBOX MATX  
  -- Electronic Arts SCHl with variable header  
  -- Electronic Arts SCHl with fixed header  
  -- Electronic Arts BNK  
  -- Electronic Arts 1SNh/EACS  
  -- generic header  
  -- Audio Interchange File Format AIFF-C  
  -- Audio Interchange File Format  
  -- .str with SNDS blocks and SHDR header  
  -- Westwood Studios .aud  
  -- Westwood Studios .aud, old style  
  -- RIFF, for WAVs  
  -- .wav + .pos for looping (Ys Complete PC)  
  -- RIFF w/ loop Markers in LIST-adtl-labl  
  -- RIFF w/ loop data in smpl chunk  
  -- RIFF w/ loop data in wsmp chunk  
  -- .mwv RIFF w/ loop data in ctrl chunk pflt  
  -- RIFX, for big-endian WAVs  
  -- RIFX w/ loop data in smpl chunk  
  -- XNA Game Studio 4.0  
  -- Lego Island MxSt  
  -- Worms 4 Mayhem SAB+SOB file  
  -- Visual Art's NWA  
  -- Visual Art's NWA w/ NWAINFO.INI for looping  
  -- Visual Art's NWA w/ Gameexe.ini for looping  
  -- Konami KCE Nagoya DVI (SAT games)  
  -- Konami KCE Yokohama KCEYCOMP (DC games)  
  -- InterPlay ACM header  
  -- MUS playlist of InterPlay ACM files  
  -- Falcom PC games (Xanadu Next, Gurumin)  
  -- Men in Black .vs  
  -- FFXI (PC) BGW  
  -- FFXI (PC) SPW  
  -- Pop'n'Music 7 Audio File  
  -- Pop'n'Music 9 Audio File  
  -- Gamecube Interleave DSP  
  -- Tekken 5 Stream Files  
  -- Gunvari MCG Files (was name .GCM on disk)  
  -- Dragon Booster ZSD  
  -- "RedSpark" RSD (MadWorld)  
  -- .ivaud GTA IV  
  -- Spider-Man 3, Tony Hawk's Downhill Jam, possibly more...  
  -- Rave Master (Groove Adventure Rave)(GC)  
  -- NAOMI/NAOMI2 ARcade games  
  -- beatmaniaIIDX16 - EMPRESS (Arcade)  
  -- beatmaniaIIDX16 - EMPRESS (Arcade)  
  -- Rune: Viking Warlord  
  -- Sim City 3000 (PC)  
  -- Scorpion King (NGC)  
  -- iPhone .caf  
  -- Activision EXAKT .SC (PS2)  
  -- DiRT 2 (WII)  
  -- Policenauts (3DO)  
  -- Policenauts (PSX)  
  -- Half Life 2 (XBOX)  
  -- Nightcaster II - Equinox (XBOX)  
  -- Turok: Evolution (NGC), Vexx (NGC)  
  -- Shooting Love. ~TRIZEAL~  
  -- raw Siren 14, 24kbit mono  
  -- raw Siren 14, 48kbit stereo  
  -- NamCollection  
  -- Homura  
  -- Psyvariar -Complete Edition-  
  -- RAW Danger (Zettaizetsumei Toshi 2 - Itetsuita Kiokutachi) [PS2]  
  -- Prototype P3D  
  -- Tekken (NamCollection)  
  -- Legacy of Kain - Blood Omen 2 (GC)  
  -- Various (2 dsp files stuck together  
  -- Big Air Freestyle, Terminator 3  
  -- Micro Machines, Superman Superman: Shadow of Apokolis  
  -- Future Cop L.A.P.D., Freekstyle  
  -- 7 Wonders of the ancient world  
  -- The golden Compass  
  -- XIII, possibly more (Ubisoft header???)  
  -- Cabelas games  
  -- Dragon Quest V (PS2)  
  -- Ah! My Goddess  
  -- Autobahn Raser - Police Madness  
  -- XPEC Entertainment (Beat Down (PS2 Xbox), Spectral Force Chronicle (PS2))  
  -- Guitar Hero III Mobile .bar  
  -- Freedom Fighters [NGC]  
  -- Sengoku Basara 3 [WII]  
  -- Tantei Jinguji Saburo - Kind of Blue (PS2)  
  -- Square-Enix SCD  
  -- Animaniacs [NGC]  
  -- Bizarre Creations (Blur, James Bond)  
  -- Ratchet & Clank Future: Quest for Booty (PS3)  
  -- Eternal Sonata (PS3)  
  -- Bakugan Battle Brawlers (PS3)  
  -- Sony: Folklore, Genji, Tokyo Jungle (PS3), Brave Story, Kurohyo (PSP)  
  -- Donkey Kong Country Returns (Wii)  
  -- Def Jam Rapstar  
  -- Ueki no Housoku - Taosu ze Robert Juudan!! (PS2)  
  -- The Bouncer  
  -- .lsf n1nj4n Fastlane Street Racing (iPhone)  
  -- The Warriors (PS2)  
  -- Hyperscan KVAG/BVG  
  -- Crash Bandicoot Nitro Kart 2 (iOS)  
  -- Excitebots .sfx  
  -- Excitebots .sf0  
  -- Metal Gear Solid 3 VAG1  
  -- Metal Gear Solid 3 VAG2  
  -- LEGO Racers (PC)  
  -- Shuffle! (PC)  
  -- Mini Ninjas (PC/PS3/WII)  
  -- Guerilla: ShellShock Nam '67 (PS2/Xbox), Killzone (PS2)  
  -- Lowrider (PS2)  
  -- Konami: Mahoromatic: Moetto - KiraKira Maid-San, GANTZ (PS2)  
  -- Disney's Stitch - Experiment 626  
  -- Otomedius (Arcade)  
  -- Nintendo 3DS CSTM (Century Stream)  
  -- Nintendo Wii U FSTM (caFe? Stream)  
  -- Koei Tecmo WiiBGM  
  -- Koei Tecmo Nintendo Stream (KNS)  
  -- Capcom MCA "MADP"  
  -- Xenoblade Chronicles 3D ADX  
  -- CRI HCA  
  -- Graffiti Kingdom  
  -- any file supported by FFmpeg  
  -- Eternal Sonata (Xbox 360)  
  -- SQEX iOS  
  -- Namco PASX (Soul Calibur II HD X360)  
  -- Microsoft RIFF XMA  
  -- Dead Rising (X360)  
  -- Audiokinetic Wwise RIFF/RIFX  
  -- Ubisoft RAKI header (Rayman Legends, Just Dance 2017)  
  -- Sony SXD (Gravity Rush, Freedom Wars PSV)  
  -- Shin'en Wii/WiiU (Jett Rocket (Wii), FAST Racing NEO (WiiU))  
  -- Paradigm games (T3 PS2, MX Rider PS2, MI: Operation Surma PS2)  
  -- Knights Contract (X360/PS3), Valhalla Knights 3 (PSV)  
  -- Burnout 1 (GC only)  
  -- generic text header  
  -- Silicon Knights .AUD (Eternal Darkness GC)  
  -- CRI AHX header  
  -- Angel Studios/Rockstar San Diego Games  
  -- RAD Game Tools BINK audio/video  
  -- Electronic Arts SNU (Dead Space)  
  -- Rockstar AWC (GTA5, RDR)  
  -- Nintendo Opus [Lego City Undercover (Switch)]  
  -- Dead Rising (PC)  
  -- Namco AAC (3DS)  
  -- Ubisoft banks  
  -- EZ2DJ (Arcade) EZWAV  
  -- Gameloft mobile games  
  -- Electronic Arts SNR+SNS (Burnout Paradise)  
  -- Electronic Arts SPS (Burnout Crash)  
  -- Ultima IX PC  
  -- Harmonix Music Systems MOGG Vorbis  
  -- Ogg Vorbis  
  -- Ogg Vorbis file w/ companion .sli for looping  
  -- Ogg Opus file w/ companion .sli for looping  
  -- Ogg Vorbis file w/ .sfl (RIFF SFPL) for looping  
  -- Ogg Vorbis with header and encryption (Koei Tecmo Games)  
  -- Ogg Vorbis with encryption  
  -- Koei Tecmo [Nobunaga no Yabou - Souzou (Vita)]  
  -- Starbreeze games  
  -- Square-Enix newest middleware (sound)  
  -- Square-Enix newest middleware (music)  
  -- KID WAF [Ever 17 (PC)]  
  -- EngineBlack games [Mighty Switch Force! (3DS)]  
  -- EngineBlack games, segmented [Shantae and the Pirate's Curse (PC)]  
  -- Cho Aniki Zero (PSP)  
  -- Nex Entertainment games [Time Crisis 4 (PS3), Time Crisis Razing Storm (PS3)]  
  -- Electronic Arts PS movies [Future Cop - L.A.P.D. (PS), Supercross 2000 (PS)]  
  -- Electronic Arts PS movies [Wing Commander 3/4 (PS)]  
  -- STHD .stx [Kakuto Chojin (Xbox)]  
  -- MP4/AAC  
  -- .PCM+SRE [Viewtiful Joe (PS2)]  
  -- Skyrim (Switch)  
  -- Ubisoft LyN engine [The Adventures of Tintin (multi)]  
  -- sfx companion of MIH+MIB  
  -- generic text playlist  
  -- Wangan Midnight (System 246)  
  -- PPST [Parappa the Rapper (PSP)]  
  -- Ubisoft BAO  
  -- Gal Gun 2 (Switch)  
  -- Hudson HVQM4 video [Resident Evil 0 (GC), Tales of Symphonia (GC)]  
  -- Argonaut ASF [Croc 2 (PC)]  
  -- Konami XMD [Silent Hill 4 (Xbox), Castlevania: Curse of Darkness (Xbox)]  
  -- Cricket Audio stream [Part Time UFO (Android), Mega Man 1-6 (Android)]  
  -- Cricket Audio bank [Fire Emblem Heroes (Android), Mega Man 1-6 (Android)]  
  -- Gorilla Systems PC games  
  -- Firebrand Games  
  -- Sony PS3 bank  
  -- Sony Scream Tool bank  
  -- Square Enix SCD old version  
  -- Penny-Punching Princess (Switch) sfx  
  -- Charinko Hero (GC)  
  -- Scooby-Doo! Unmasked (PS2)  
  -- Headhunter (PS2)  
  -- Hunter - The Reckoning - Wayward (PS2)  
  -- AirForce Delta Strike (PS2)  
  -- Ratatouille (GC)  
  -- Ratatouille (PC)  
  -- Cloudphobia (PC)  
  -- MegaRace 3 (PC)  
  -- Slave Zero (PC)  
  -- Yu-Gi-Oh - The Dawn of Destiny (Xbox)  
  -- Stupid Invaders (PC)  
  -- DDR Supernova 2 AC  
   type meta_t is 
     (meta_SILENCE,
      meta_DSP_STD,
      meta_DSP_CSTR,
      meta_DSP_RS03,
      meta_DSP_STM,
      meta_AGSC,
      meta_CSMP,
      meta_RFRM,
      meta_DSP_MPDSP,
      meta_DSP_JETTERS,
      meta_DSP_MSS,
      meta_DSP_GCM,
      meta_DSP_STR,
      meta_DSP_SADB,
      meta_DSP_WSI,
      meta_IDSP_TT,
      meta_DSP_WII_MUS,
      meta_DSP_WII_WSD,
      meta_WII_NDP,
      meta_DSP_YGO,
      meta_STRM,
      meta_RSTM,
      meta_AFC,
      meta_AST,
      meta_RWSD,
      meta_RWAR,
      meta_RWAV,
      meta_CWAV,
      meta_FWAV,
      meta_RSTM_SPM,
      meta_THP,
      meta_RSTM_shrunken,
      meta_SWAV,
      meta_NDS_RRDS,
      meta_WII_BNS,
      meta_WIIU_BTSND,
      meta_ADX_03,
      meta_ADX_04,
      meta_ADX_05,
      meta_AIX,
      meta_AAX,
      meta_UTF_DSP,
      meta_DTK,
      meta_RSF,
      meta_HALPST,
      meta_GCSW,
      meta_CAF,
      meta_MYSPD,
      meta_HIS,
      meta_BNSF,
      meta_XA,
      meta_ADS,
      meta_NPS,
      meta_RXWS,
      meta_RAW_INT,
      meta_EXST,
      meta_SVAG_KCET,
      meta_PS_HEADERLESS,
      meta_MIB_MIH,
      meta_PS2_MIC,
      meta_PS2_VAGi,
      meta_PS2_VAGp,
      meta_PS2_pGAV,
      meta_PS2_VAGp_AAAP,
      meta_SEB,
      meta_STR_WAV,
      meta_ILD,
      meta_PS2_PNB,
      meta_VPK,
      meta_PS2_BMDX,
      meta_PS2_IVB,
      meta_PS2_SND,
      meta_SVS,
      meta_XSS,
      meta_SL3,
      meta_HGC1,
      meta_AUS,
      meta_RWS,
      meta_FSB1,
      meta_FSB2,
      meta_FSB3,
      meta_FSB4,
      meta_FSB5,
      meta_RWX,
      meta_XWB,
      meta_PS2_XA30,
      meta_MUSC,
      meta_MUSX,
      meta_LEG,
      meta_FILP,
      meta_IKM,
      meta_STER,
      meta_BG00,
      meta_PS2_RSTM,
      meta_PS2_KCES,
      meta_PS2_DXH,
      meta_VSV,
      meta_SCD_PCM,
      meta_PS2_PCM,
      meta_PS2_RKV,
      meta_PS2_VAS,
      meta_PS2_TEC,
      meta_PS2_ENTH,
      meta_SDT,
      meta_NGC_TYDSP,
      meta_CAPDSP,
      meta_DC_STR,
      meta_DC_STR_V2,
      meta_NGC_BH2PCM,
      meta_SAP,
      meta_DC_IDVI,
      meta_KRAW,
      meta_PS2_OMU,
      meta_PS2_XA2,
      meta_NUB,
      meta_IDSP_NL,
      meta_IDSP_IE,
      meta_SPT_SPD,
      meta_ISH_ISD,
      meta_GSP_GSB,
      meta_YDSP,
      meta_FFCC_STR,
      meta_UBI_JADE,
      meta_GCA,
      meta_NGC_SSM,
      meta_PS2_JOE,
      meta_NGC_YMF,
      meta_SADL,
      meta_PS2_CCC,
      meta_FAG,
      meta_PS2_MIHB,
      meta_NGC_PDT,
      meta_DC_ASD,
      meta_NAOMI_SPSD,
      meta_RSD,
      meta_PS2_ASS,
      meta_SEG,
      meta_NDS_STRM_FFTA2,
      meta_KNON,
      meta_ZWDSP,
      meta_VGS,
      meta_DCS_WAV,
      meta_SMP,
      meta_WII_SNG,
      meta_MUL,
      meta_SAT_BAKA,
      meta_VSF,
      meta_PS2_VSF_TTA,
      meta_ADS_MIDWAY,
      meta_PS2_SPS,
      meta_PS2_XA2_RRP,
      meta_NGC_DSP_KONAMI,
      meta_UBI_CKD,
      meta_RAW_WAVM,
      meta_WVS,
      meta_XBOX_MATX,
      meta_XMU,
      meta_XVAS,
      meta_EA_SCHL,
      meta_EA_SCHL_fixed,
      meta_EA_BNK,
      meta_EA_1SNH,
      meta_EA_EACS,
      meta_RAW_PCM,
      meta_GENH,
      meta_AIFC,
      meta_AIFF,
      meta_STR_SNDS,
      meta_WS_AUD,
      meta_WS_AUD_old,
      meta_RIFF_WAVE,
      meta_RIFF_WAVE_POS,
      meta_RIFF_WAVE_labl,
      meta_RIFF_WAVE_smpl,
      meta_RIFF_WAVE_wsmp,
      meta_RIFF_WAVE_MWV,
      meta_RIFX_WAVE,
      meta_RIFX_WAVE_smpl,
      meta_XNB,
      meta_PC_MXST,
      meta_SAB,
      meta_NWA,
      meta_NWA_NWAINFOINI,
      meta_NWA_GAMEEXEINI,
      meta_SAT_DVI,
      meta_DC_KCEY,
      meta_ACM,
      meta_MUS_ACM,
      meta_DEC,
      meta_VS,
      meta_FFXI_BGW,
      meta_FFXI_SPW,
      meta_STS,
      meta_PS2_P2BT,
      meta_PS2_GBTS,
      meta_NGC_DSP_IADP,
      meta_PS2_TK5,
      meta_PS2_MCG,
      meta_ZSD,
      meta_REDSPARK,
      meta_IVAUD,
      meta_NDS_HWAS,
      meta_NGC_LPS,
      meta_NAOMI_ADPCM,
      meta_SD9,
      meta_2DX9,
      meta_PS2_VGV,
      meta_GCUB,
      meta_MAXIS_XA,
      meta_NGC_SCK_DSP,
      meta_CAFF,
      meta_EXAKT_SC,
      meta_WII_WAS,
      meta_PONA_3DO,
      meta_PONA_PSX,
      meta_XBOX_HLWAV,
      meta_AST_MV,
      meta_AST_MMV,
      meta_DMSG,
      meta_NGC_DSP_AAAP,
      meta_PS2_WB,
      meta_S14,
      meta_SSS,
      meta_PS2_GCM,
      meta_PS2_SMPL,
      meta_PS2_MSA,
      meta_PS2_VOI,
      meta_P3D,
      meta_PS2_TK1,
      meta_NGC_RKV,
      meta_DSP_DDSP,
      meta_NGC_DSP_MPDS,
      meta_DSP_STR_IG,
      meta_EA_SWVR,
      meta_PS2_B1S,
      meta_PS2_WAD,
      meta_DSP_XIII,
      meta_DSP_CABELAS,
      meta_PS2_ADM,
      meta_LPCM_SHADE,
      meta_DSP_BDSP,
      meta_PS2_VMS,
      meta_XAU,
      meta_GH3_BAR,
      meta_FFW,
      meta_DSP_DSPW,
      meta_PS2_JSTM,
      meta_SQEX_SCD,
      meta_NGC_NST_DSP,
      meta_BAF,
      meta_XVAG,
      meta_PS3_CPS,
      meta_MSF,
      meta_PS3_PAST,
      meta_SGXD,
      meta_WII_RAS,
      meta_SPM,
      meta_X360_TRA,
      meta_VGS_PS,
      meta_PS2_IAB,
      meta_VS_STR,
      meta_LSF_N1NJ4N,
      meta_XWAV,
      meta_RAW_SNDS,
      meta_PS2_WMUS,
      meta_HYPERSCAN_KVAG,
      meta_IOS_PSND,
      meta_BOS_ADP,
      meta_QD_ADP,
      meta_EB_SFX,
      meta_EB_SF0,
      meta_MTAF,
      meta_PS2_VAG1,
      meta_PS2_VAG2,
      meta_TUN,
      meta_WPD,
      meta_MN_STR,
      meta_MSS,
      meta_PS2_HSF,
      meta_IVAG,
      meta_PS2_2PFS,
      meta_PS2_VBK,
      meta_OTM,
      meta_CSTM,
      meta_FSTM,
      meta_IDSP_NAMCO,
      meta_KT_WIIBGM,
      meta_KTSS,
      meta_MCA,
      meta_XB3D_ADX,
      meta_HCA,
      meta_SVAG_SNK,
      meta_PS2_VDS_VDM,
      meta_FFMPEG,
      meta_X360_CXS,
      meta_AKB,
      meta_X360_PASX,
      meta_XMA_RIFF,
      meta_X360_AST,
      meta_WWISE_RIFF,
      meta_UBI_RAKI,
      meta_SXD,
      meta_OGL,
      meta_MC3,
      meta_GTD,
      meta_TA_AAC,
      meta_MTA2,
      meta_NGC_ULW,
      meta_XA_XA30,
      meta_XA_04SW,
      meta_TXTH,
      meta_SK_AUD,
      meta_AHX,
      meta_STM,
      meta_BINK,
      meta_EA_SNU,
      meta_AWC,
      meta_OPUS,
      meta_RAW_AL,
      meta_PC_AST,
      meta_NAAC,
      meta_UBI_SB,
      meta_EZW,
      meta_VXN,
      meta_EA_SNR_SNS,
      meta_EA_SPS,
      meta_VID1,
      meta_PC_FLX,
      meta_MOGG,
      meta_OGG_VORBIS,
      meta_OGG_SLI,
      meta_OPUS_SLI,
      meta_OGG_SFL,
      meta_OGG_KOVS,
      meta_OGG_encrypted,
      meta_KMA9,
      meta_XWC,
      meta_SQEX_SAB,
      meta_SQEX_MAB,
      meta_WAF,
      meta_WAVE,
      meta_WAVE_segmented,
      meta_SMV,
      meta_NXAP,
      meta_EA_WVE_AU00,
      meta_EA_WVE_AD10,
      meta_STHD,
      meta_MP4,
      meta_PCM_SRE,
      meta_DSP_MCADPCM,
      meta_UBI_LYN,
      meta_MSB_MSH,
      meta_TXTP,
      meta_SMC_SMH,
      meta_PPST,
      meta_SPS_N1,
      meta_UBI_BAO,
      meta_DSP_SWITCH_AUDIO,
      meta_H4M,
      meta_ASF,
      meta_XMD,
      meta_CKS,
      meta_CKB,
      meta_WV6,
      meta_WAVEBATCH,
      meta_HD3_BD3,
      meta_BNK_SONY,
      meta_SCD_SSCF,
      meta_DSP_VAG,
      meta_DSP_ITL,
      meta_A2M,
      meta_AHV,
      meta_MSV,
      meta_SDF,
      meta_SVG,
      meta_VIS,
      meta_VAI,
      meta_AIF_ASOBO,
      meta_AO,
      meta_APC,
      meta_WV2,
      meta_XAU_KONAMI,
      meta_DERF,
      meta_SADF,
      meta_UTK,
      meta_NXA,
      meta_ADPCM_CAPCOM,
      meta_UE4OPUS,
      meta_XWMA,
      meta_VA3,
      meta_XOPUS,
      meta_VS_SQUARE,
      meta_NWAV,
      meta_XPCM,
      meta_MSF_TAMASOFT,
      meta_XPS_DAT,
      meta_ZSND,
      meta_DSP_ADPY,
      meta_DSP_ADPX,
      meta_OGG_OPUS,
      meta_IMC,
      meta_GIN,
      meta_DSF,
      meta_208,
      meta_DSP_DS2,
      meta_MUS_VC,
      meta_STRM_ABYLIGHT,
      meta_MSF_KONAMI,
      meta_XWMA_KONAMI,
      meta_9TAV,
      meta_BWAV,
      meta_RAD,
      meta_SMACKER,
      meta_MZRT,
      meta_XAVS,
      meta_PSF,
      meta_DSP_ITL_i,
      meta_IMA,
      meta_XMV_VALVE,
      meta_UBI_HX,
      meta_BMP_KONAMI,
      meta_ISB,
      meta_XSSB,
      meta_XMA_UE3,
      meta_FWSE,
      meta_FDA,
      meta_TGC,
      meta_KWB,
      meta_LRMD,
      meta_WWISE_FX,
      meta_DIVA,
      meta_IMUSE,
      meta_KTSR,
      meta_KAT,
      meta_PCM_SUCCESS,
      meta_ADP_KONAMI,
      meta_SDRH,
      meta_WADY,
      meta_DSP_SQEX,
      meta_DSP_WIIVOICE,
      meta_SBK,
      meta_DSP_WIIADPCM,
      meta_DSP_CWAC,
      meta_COMPRESSWAVE,
      meta_KTAC,
      meta_MJB_MJH,
      meta_BSNF,
      meta_TAC,
      meta_IDSP_TOSE,
      meta_DSP_KWA,
      meta_OGV_3RDEYE,
      meta_PIFF_TPCM,
      meta_WXD_WXH,
      meta_BNK_RELIC,
      meta_XSH_XSD_XSS,
      meta_PSB,
      meta_LOPU_FB,
      meta_LPCM_FB,
      meta_WBK,
      meta_WBK_NSLB,
      meta_DSP_APEX,
      meta_MPEG)
   with Convention => C;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:758

  -- standard WAVEFORMATEXTENSIBLE speaker positions  
  -- front left  
  -- front right  
  -- front center  
  -- low frequency effects  
  -- back left  
  -- back right  
  -- front left center  
  -- front right center  
  -- back center  
  -- side left  
  -- side right  
  -- top center 
  -- top front left  
  -- top front center  
  -- top front right  
  -- top back left  
  -- top back center  
  -- top back left  
   subtype speaker_t is unsigned;
   speaker_FL : constant speaker_t := 1;
   speaker_FR : constant speaker_t := 2;
   speaker_FC : constant speaker_t := 4;
   speaker_LFE : constant speaker_t := 8;
   speaker_BL : constant speaker_t := 16;
   speaker_BR : constant speaker_t := 32;
   speaker_FLC : constant speaker_t := 64;
   speaker_FRC : constant speaker_t := 128;
   speaker_BC : constant speaker_t := 256;
   speaker_SL : constant speaker_t := 512;
   speaker_SR : constant speaker_t := 1024;
   speaker_TC : constant speaker_t := 2048;
   speaker_TFL : constant speaker_t := 4096;
   speaker_TFC : constant speaker_t := 8192;
   speaker_TFR : constant speaker_t := 16384;
   speaker_TBL : constant speaker_t := 32768;
   speaker_TBC : constant speaker_t := 65536;
   speaker_TBR : constant speaker_t := 131072;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:782

  -- typical mappings that metas may use to set channel_layout (but plugin must actually use it)
  -- * (in order, so 3ch file could be mapped to FL FR FC or FL FR LFE but not LFE FL FR)
  -- * not too sure about names but no clear standards  

  -- aka 3STEREO?  
   subtype mapping_t is unsigned;
   mapping_MONO : constant mapping_t := 4;
   mapping_STEREO : constant mapping_t := 3;
   mapping_2POINT1 : constant mapping_t := 11;
   mapping_2POINT1_xiph : constant mapping_t := 7;
   mapping_QUAD : constant mapping_t := 51;
   mapping_QUAD_surround : constant mapping_t := 263;
   mapping_QUAD_side : constant mapping_t := 1539;
   mapping_5POINT0 : constant mapping_t := 59;
   mapping_5POINT0_xiph : constant mapping_t := 55;
   mapping_5POINT0_surround : constant mapping_t := 1543;
   mapping_5POINT1 : constant mapping_t := 63;
   mapping_5POINT1_surround : constant mapping_t := 1551;
   mapping_7POINT0 : constant mapping_t := 463;
   mapping_7POINT1 : constant mapping_t := 255;
   mapping_7POINT1_surround : constant mapping_t := 1599;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:803

  -- some of the mods below are set  
  -- modifiers  
  -- processing  
  -- not in samples for backwards compatibility  
  --double fade_delay_s;
  --double fade_time_s;
  -- internal flags  
  -- for lack of a better place...  
   type play_config_t is record
      config_set : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:806
      play_forever : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:809
      ignore_loop : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:810
      force_loop : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:811
      really_force_loop : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:812
      ignore_fade : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:813
      loop_count : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:816
      pad_begin : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:817
      trim_begin : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:818
      body_time : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:819
      trim_end : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:820
      fade_delay : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:821
      fade_time : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:822
      pad_end : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:823
      pad_begin_s : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:825
      trim_begin_s : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:826
      body_time_s : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:827
      trim_end_s : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:828
      pad_end_s : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:831
      pad_begin_set : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:834
      trim_begin_set : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:835
      body_time_set : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:836
      loop_count_set : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:837
      trim_end_set : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:838
      fade_delay_set : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:839
      fade_time_set : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:840
      pad_end_set : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:841
      is_txtp : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:844
      is_mini_txtp : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:845
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:847

  --int32_t pad_end_left;
  -- total samples that the stream lasts (after applying all config)  
  -- absolute sample where stream is  
   type play_state_t is record
      input_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:851
      output_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:852
      pad_begin_duration : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:854
      pad_begin_left : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:855
      trim_begin_duration : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:856
      trim_begin_left : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:857
      body_duration : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:858
      fade_duration : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:859
      fade_left : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:860
      fade_start : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:861
      pad_end_duration : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:862
      pad_end_start : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:864
      play_duration : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:866
      play_position : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:867
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:869

  -- info for a single vgmstream channel  
  -- file used by this channel  
  -- where data for this channel begins  
  -- current location in the file  
  -- offset of the current frame header (for WS)  
  -- for WS  
  -- format specific  
  -- adpcm  
  -- formats with decode coefficients built in (DSP, some ADX)  
  -- Level-5 0x555  
  -- VADPCM: max 8 groups * max 2 order * fixed 8 subframe coefs  
  -- previous sample  
  -- previous previous sample  
  --double adpcm_history1_double;
  --double adpcm_history2_double;
  -- for IMA  
  -- for MS ADPCM  
  -- state for G.721 decoder, sort of big but we might as well keep it around  
  -- ADX encryption  
   type VGMSTREAMCHANNEL_array1675 is array (0 .. 15) of aliased bits_stdint_intn_h.int16_t;
   type VGMSTREAMCHANNEL_array1678 is array (0 .. 95) of aliased bits_stdint_intn_h.int32_t;
   type VGMSTREAMCHANNEL_array1681 is array (0 .. 127) of aliased bits_stdint_intn_h.int16_t;
   type VGMSTREAMCHANNEL_union1683 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            adpcm_history1_16 : aliased bits_stdint_intn_h.int16_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:888
         when others =>
            adpcm_history1_32 : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:889
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type VGMSTREAMCHANNEL_union1684 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            adpcm_history2_16 : aliased bits_stdint_intn_h.int16_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:892
         when others =>
            adpcm_history2_32 : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:893
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type VGMSTREAMCHANNEL_union1685 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            adpcm_history3_16 : aliased bits_stdint_intn_h.int16_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:896
         when others =>
            adpcm_history3_32 : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:897
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type VGMSTREAMCHANNEL_union1686 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            adpcm_history4_16 : aliased bits_stdint_intn_h.int16_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:900
         when others =>
            adpcm_history4_32 : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:901
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type VGMSTREAMCHANNEL is record
      the_streamfile : access streamfile_h.STREAMFILE;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:874
      channel_start_offset : aliased sys_types_h.off_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:875
      offset : aliased sys_types_h.off_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:876
      frame_header_offset : aliased sys_types_h.off_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:878
      samples_left_in_frame : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:879
      adpcm_coef : aliased VGMSTREAMCHANNEL_array1675;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:884
      adpcm_coef_3by32 : aliased VGMSTREAMCHANNEL_array1678;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:885
      vadpcm_coefs : aliased VGMSTREAMCHANNEL_array1681;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:886
      anon4302 : aliased VGMSTREAMCHANNEL_union1683;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:890
      anon4306 : aliased VGMSTREAMCHANNEL_union1684;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:894
      anon4310 : aliased VGMSTREAMCHANNEL_union1685;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:898
      anon4314 : aliased VGMSTREAMCHANNEL_union1686;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:902
      adpcm_step_index : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:907
      adpcm_scale : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:908
      the_g72x_state : aliased g72x_state_h.g72x_state;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:911
      adx_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:914
      adx_xor : aliased bits_stdint_uintn_h.uint16_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:915
      adx_mult : aliased bits_stdint_uintn_h.uint16_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:916
      adx_add : aliased bits_stdint_uintn_h.uint16_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:917
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:919

  -- main vgmstream info  
  -- basic config  
  -- the actual max number of samples  
  -- sample rate in Hz  
  -- number of channels  
  -- type of encoding  
  -- type of layout  
  -- type of metadata  
  -- loopin config  
  -- is this stream looped?  
  -- first sample of the loop (included in the loop)  
  -- last sample of the loop (not included in the loop)  
  -- layouts/block config  
  -- interleave, or block/frame size (depending on the codec)  
  -- different interleave for first block  
  -- data skipped before interleave first (needed to skip other channels)  
  -- smaller interleave for last block  
  -- for codecs with configurable size  
  -- subsong config  
  -- for multi-stream formats (0=not set/one stream, 1=one stream)  
  -- selected subsong (also 1-based)  
  -- info to properly calculate bitrate in case of subsongs  
  -- name of the current stream (info), if the file stores it and it's filled  
  -- mapping config (info for plugins)  
  -- order: FL FR FC LFE BL BR FLC FRC BC SL SR etc (WAVEFORMATEX flags where FL=lowest bit set)  
  -- other config  
  -- search for dual stereo (file_L.ext + file_R.ext = single stereo file)  
  -- layout/block state  
  -- actual data size of an entire block (ie. may be fixed, include padding/headers, etc)  
  -- sample point within the file (for loop detection)  
  -- number of samples into the current block/interleave/segment/etc  
  -- start of this block (offset of block header)  
  -- size in usable bytes of the block we're in now (used to calculate num_samples per block)  
  -- size in samples of the block we're in now (used over current_block_size if possible)  
  -- offset of header of the next block  
  -- loop state (saved when loop is hit to restore later)  
  -- saved from current_sample (same as loop_start_sample, but more state-like)  
  -- saved from samples_into_block  
  -- saved from current_block_offset  
  -- saved from current_block_size  
  -- saved from current_block_samples  
  -- saved from next_block_offset  
  -- save config when loop is hit, but first time only  
  -- decoder config/state  
  -- little/big endian marker; name is left vague but usually means big endian  
  -- flags for codecs or layouts with minor variations; meaning is up to them  
  -- WS ADPCM: output bytes for this block  
  -- main state  
  -- array of channels  
  -- shallow copy of channels as they were at the beginning of the stream (for resets)  
  -- shallow copy of channels as they were at the loop point (for loops)  
  -- shallow copy of the VGMSTREAM as it was at the beginning of the stream (for resets)  
  -- state for mixing effects  
  -- Optional data the codec needs for the whole stream. This is for codecs too
  --     * different from vgmstream's structure to be reasonably shoehorned.
  --     * Note also that support must be added for resetting, looping and
  --     * closing for every codec that uses this, as it will not be handled.  

  -- Same, for special layouts. layout_data + codec_data may exist at the same time.  
  -- play config/state  
  -- config can be used  
  -- player config (applied over decoding)  
  -- player state (applied over decoding)  
  -- counter of complete loops (1=looped once)  
  -- max loops before continuing with the stream end (loops forever if not set)  
  -- garbage buffer used for seeking/trimming  
  -- for all channels (samples = tmpbuf_size / channels)  
   subtype VGMSTREAM_array1690 is Interfaces.C.char_array (0 .. 254);
   type VGMSTREAM is record
      num_samples : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:925
      sample_rate : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:926
      channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:927
      coding_type : aliased coding_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:928
      layout_type : aliased layout_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:929
      meta_type : aliased meta_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:930
      loop_flag : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:933
      loop_start_sample : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:934
      loop_end_sample : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:935
      interleave_block_size : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:938
      interleave_first_block_size : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:939
      interleave_first_skip : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:940
      interleave_last_block_size : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:941
      frame_size : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:942
      num_streams : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:945
      stream_index : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:946
      stream_size : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:947
      stream_name : aliased VGMSTREAM_array1690;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:948
      channel_layout : aliased bits_stdint_uintn_h.uint32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:951
      allow_dual_stereo : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:954
      full_block_size : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:958
      current_sample : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:959
      samples_into_block : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:960
      current_block_offset : aliased sys_types_h.off_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:961
      current_block_size : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:962
      current_block_samples : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:963
      next_block_offset : aliased sys_types_h.off_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:964
      loop_current_sample : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:967
      loop_samples_into_block : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:968
      loop_block_offset : aliased sys_types_h.off_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:969
      loop_block_size : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:970
      loop_block_samples : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:971
      loop_next_block_offset : aliased sys_types_h.off_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:972
      hit_loop : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:973
      codec_endian : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:977
      codec_config : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:978
      ws_output_size : aliased bits_stdint_intn_h.int32_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:979
      ch : access VGMSTREAMCHANNEL;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:983
      start_ch : access VGMSTREAMCHANNEL;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:984
      loop_ch : access VGMSTREAMCHANNEL;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:985
      start_vgmstream : System.Address;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:986
      mixing_data : System.Address;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:988
      codec_data : System.Address;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:994
      layout_data : System.Address;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:996
      config_enabled : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1000
      config : aliased play_config_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1001
      pstate : aliased play_state_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1002
      loop_count : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1003
      loop_target : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1004
      tmpbuf : access streamtypes_h.sample_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1005
      tmpbuf_size : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1006
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1008

  -- for files made of "continuous" segments, one per section of a song (using a complete sub-VGMSTREAM)  
  -- internal buffer channels  
  -- resulting channels (after mixing, if applied)  
  -- segments have different number of channels  
   type segmented_layout_data is record
      segment_count : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1013
      segments : System.Address;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1014
      current_segment : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1015
      buffer : access streamtypes_h.sample_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1016
      input_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1017
      output_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1018
      mixed_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1019
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1020

  -- for files made of "parallel" layers, one per group of channels (using a complete sub-VGMSTREAM)  
  -- internal buffer channels  
  -- resulting channels (after mixing, if applied)  
  -- don't loop using per-layer loops, but layout's own looping  
   type layered_layout_data is record
      layer_count : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1024
      layers : System.Address;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1025
      buffer : access streamtypes_h.sample_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1026
      input_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1027
      output_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1028
      external_looping : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1029
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1030

  -- libacm interface  
   type acm_codec_data is record
      the_streamfile : access streamfile_h.STREAMFILE;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1036
      handle : System.Address;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1037
      io_config : System.Address;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1038
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1039

  -- VGMStream description in structure format
   type mixing_info is record
      input_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1067
      output_channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1068
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1066

   type loop_info is record
      start : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1072
      c_end : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1073
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1071

   type interleave_info is record
      value : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1079
      first_block : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1080
      last_block : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1081
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1078

   subtype anon1709_array1707 is Interfaces.C.char_array (0 .. 127);
   type stream_info is record
      current : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1087
      total : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1088
      name : aliased anon1709_array1707;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1089
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1086

   subtype vgmstream_info_array1707 is Interfaces.C.char_array (0 .. 127);
   type vgmstream_info is record
      sample_rate : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1064
      channels : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1065
      the_mixing_info : aliased mixing_info;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1069
      channel_layout : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1070
      the_loop_info : aliased loop_info;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1074
      num_samples : aliased stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1075
      encoding : aliased vgmstream_info_array1707;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1076
      layout : aliased vgmstream_info_array1707;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1077
      the_interleave_info : aliased interleave_info;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1082
      frame_size : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1083
      metadata : aliased vgmstream_info_array1707;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1084
      bitrate : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1085
      the_stream_info : aliased stream_info;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1090
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1091

  -- ------------------------------------------------------------------------- 
  -- vgmstream "public" API                                                    
  -- ------------------------------------------------------------------------- 
  -- do format detection, return pointer to a usable VGMSTREAM, or NULL on failure  
   function init_vgmstream (filename : Interfaces.C.Strings.chars_ptr) return access VGMSTREAM  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1098
   with Import => True, 
        Convention => C, 
        External_Name => "init_vgmstream";

  -- init with custom IO via streamfile  
   function init_vgmstream_from_STREAMFILE (sf : access streamfile_h.STREAMFILE) return access VGMSTREAM  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1101
   with Import => True, 
        Convention => C, 
        External_Name => "init_vgmstream_from_STREAMFILE";

  -- reset a VGMSTREAM to start of stream  
   procedure reset_vgmstream (the_vgmstream : access VGMSTREAM)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1104
   with Import => True, 
        Convention => C, 
        External_Name => "reset_vgmstream";

  -- close an open vgmstream  
   procedure close_vgmstream (the_vgmstream : access VGMSTREAM)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1107
   with Import => True, 
        Convention => C, 
        External_Name => "close_vgmstream";

  -- calculate the number of samples to be played based on looping parameters  
   function get_vgmstream_play_samples
     (looptimes : double;
      fadeseconds : double;
      fadedelayseconds : double;
      the_vgmstream : access VGMSTREAM) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1110
   with Import => True, 
        Convention => C, 
        External_Name => "get_vgmstream_play_samples";

  -- Decode data into sample buffer. Returns < sample_count on stream end  
   pragma Warnings (Off, "-gnatwx"); -- C-style pointer issues
   function render_vgmstream
     (buffer : Sample_Buffer_Access;
      sample_count : bits_stdint_intn_h.int32_t;
      the_vgmstream : access VGMSTREAM) return int  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1113
   with Import => True, 
        Convention => C, 
        External_Name => "render_vgmstream";
   pragma Warnings (On);

  -- Seek to sample position (next render starts from that point). Use only after config is set (vgmstream_apply_config)  
   procedure seek_vgmstream (the_vgmstream : access VGMSTREAM; seek_sample : bits_stdint_intn_h.int32_t)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1116
   with Import => True, 
        Convention => C, 
        External_Name => "seek_vgmstream";

  -- Write a description of the stream into array pointed by desc, which must be length bytes long.
  -- * Will always be null-terminated if length > 0  

   procedure describe_vgmstream
     (the_vgmstream : access VGMSTREAM;
      desc : Interfaces.C.Strings.chars_ptr;
      length : int)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1120
   with Import => True, 
        Convention => C, 
        External_Name => "describe_vgmstream";

   procedure describe_vgmstream_info (the_vgmstream : access VGMSTREAM; desc : access vgmstream_info)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1121
   with Import => True, 
        Convention => C, 
        External_Name => "describe_vgmstream_info";

  -- Return the average bitrate in bps of all unique files contained within this stream.  
   function get_vgmstream_average_bitrate (the_vgmstream : access VGMSTREAM) return int  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1124
   with Import => True, 
        Convention => C, 
        External_Name => "get_vgmstream_average_bitrate";

  -- List supported formats and return elements in the list, for plugins that need to know.
  -- * The list disables some common formats that may conflict (.wav, .ogg, etc).  

   function vgmstream_get_formats (size : access stddef_h.size_t) return System.Address  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1128
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_get_formats";

  -- same, but for common-but-disabled formats in the above list.  
   function vgmstream_get_common_formats (size : access stddef_h.size_t) return System.Address  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1131
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_get_common_formats";

  -- Force enable/disable internal looping. Should be done before playing anything (or after reset),
  -- * and not all codecs support arbitrary loop values ATM.  

   procedure vgmstream_force_loop
     (the_vgmstream : access VGMSTREAM;
      loop_flag : int;
      loop_start_sample : int;
      loop_end_sample : int)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1135
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_force_loop";

  -- Set number of max loops to do, then play up to stream end (for songs with proper endings)  
   procedure vgmstream_set_loop_target (the_vgmstream : access VGMSTREAM; loop_target : int)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1138
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_set_loop_target";

  -- Return 1 if vgmstream detects from the filename that said file can be used even if doesn't physically exist  
   function vgmstream_is_virtual_filename (filename : Interfaces.C.Strings.chars_ptr) return int  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1141
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_is_virtual_filename";

  -- ------------------------------------------------------------------------- 
  -- vgmstream "private" API                                                   
  -- ------------------------------------------------------------------------- 
  -- Allocate initial memory for the VGMSTREAM  
   function allocate_vgmstream (channel_count : int; looped : int) return access VGMSTREAM  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1148
   with Import => True, 
        Convention => C, 
        External_Name => "allocate_vgmstream";

  -- Prepare the VGMSTREAM's initial state once parsed and ready, but before playing.  
   procedure setup_vgmstream (the_vgmstream : access VGMSTREAM)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1151
   with Import => True, 
        Convention => C, 
        External_Name => "setup_vgmstream";

  -- Open the stream for reading at offset (taking into account layouts, channels and so on).
  -- * Returns 0 on failure  

   function vgmstream_open_stream
     (the_vgmstream : access VGMSTREAM;
      sf : access streamfile_h.STREAMFILE;
      start_offset : sys_types_h.off_t) return int  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1155
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_open_stream";

   function vgmstream_open_stream_bf
     (the_vgmstream : access VGMSTREAM;
      sf : access streamfile_h.STREAMFILE;
      start_offset : sys_types_h.off_t;
      force_multibuffer : int) return int  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1156
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_open_stream_bf";

  -- Get description info  
   procedure get_vgmstream_coding_description
     (the_vgmstream : access VGMSTREAM;
      c_out : Interfaces.C.Strings.chars_ptr;
      out_size : stddef_h.size_t)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1159
   with Import => True, 
        Convention => C, 
        External_Name => "get_vgmstream_coding_description";

   procedure get_vgmstream_layout_description
     (the_vgmstream : access VGMSTREAM;
      c_out : Interfaces.C.Strings.chars_ptr;
      out_size : stddef_h.size_t)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1160
   with Import => True, 
        Convention => C, 
        External_Name => "get_vgmstream_layout_description";

   procedure get_vgmstream_meta_description
     (the_vgmstream : access VGMSTREAM;
      c_out : Interfaces.C.Strings.chars_ptr;
      out_size : stddef_h.size_t)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1161
   with Import => True, 
        Convention => C, 
        External_Name => "get_vgmstream_meta_description";

   procedure setup_state_vgmstream (the_vgmstream : access VGMSTREAM)  -- /home/andrew/src/vgmstream-r1721/src/vgmstream.h:1163
   with Import => True, 
        Convention => C, 
        External_Name => "setup_state_vgmstream";

end vgmstream_h;
