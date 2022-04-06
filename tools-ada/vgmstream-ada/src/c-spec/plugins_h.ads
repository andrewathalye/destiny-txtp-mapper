pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with vgmstream_h;
with bits_stdint_intn_h;
with System;
with streamfile_h;

package plugins_h is

  -- * plugins.h - helper for plugins
  --  

  --todo rename to api.h once public enough
  -- define standard C param call and name mangling (to avoid __stdcall / .defs)  
  --#define VGMSTREAM_CALL __cdecl //needed?
  -- define external function types (during compilation)  
  --VGMSTREAM_API void VGMSTREAM_CALL vgmstream_function(void);
  -- ******************************************  
  -- CONTEXT: simplifies plugin code             
  -- ******************************************  
  -- set if filename is already an extension  
  -- set if shouldn't check standard formats  
  -- set if player can't play extensionless files  
  -- set to allow any extension (for txth)  
  -- set to allow known-but-common extension (when player has plugin priority)  
   type vgmstream_ctx_valid_cfg is record
      is_extension : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:34
      skip_standard : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:35
      reject_extensionless : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:36
      accept_unknown : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:37
      accept_common : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:38
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:39

  -- returns if vgmstream can parse file by extension  
   function vgmstream_ctx_is_valid (filename : Interfaces.C.Strings.chars_ptr; cfg : access vgmstream_ctx_valid_cfg) return int  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_ctx_is_valid";

  -- song mofidiers  
  -- keeps looping forever (needs loop points)  
  -- ignores loops points  
  -- enables full loops (0..samples) if file doesn't have loop points  
  -- forces full loops even if file has loop points  
  --  don't fade after N loops  
  -- song processing  
  -- target loops  
  -- fade delay after target loops  
  -- fade period after target loops  
  --int downmix;                /* max number of channels allowed (0=disable downmix)  
   type vgmstream_cfg_t is record
      allow_play_forever : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:46
      disable_config_override : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:47
      play_forever : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:50
      ignore_loop : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:51
      force_loop : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:52
      really_force_loop : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:53
      ignore_fade : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:54
      loop_count : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:57
      fade_delay : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:58
      fade_time : aliased double;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:59
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:63

  -- WARNING: these are not stable and may change anytime without notice
   procedure vgmstream_apply_config (the_vgmstream : access vgmstream_h.VGMSTREAM; pcfg : access vgmstream_cfg_t)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_apply_config";

   function vgmstream_get_samples (the_vgmstream : access vgmstream_h.VGMSTREAM) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_get_samples";

   function vgmstream_get_play_forever (the_vgmstream : access vgmstream_h.VGMSTREAM) return int  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_get_play_forever";

   procedure vgmstream_set_play_forever (the_vgmstream : access vgmstream_h.VGMSTREAM; enabled : int)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:69
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_set_play_forever";

   type vgmstream_title_t is record
      force_title : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:73
      subsong_range : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:74
      remove_extension : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:75
      remove_archive : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:76
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:77

  -- get a simple title for plugins  
   procedure vgmstream_get_title
     (buf : Interfaces.C.Strings.chars_ptr;
      buf_len : int;
      filename : Interfaces.C.Strings.chars_ptr;
      the_vgmstream : access vgmstream_h.VGMSTREAM;
      cfg : access vgmstream_title_t)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_get_title";

  -- CB: void (*callback)(int level, const char* str)
   procedure vgmstream_set_log_callback (level : int; callback : System.Address)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_set_log_callback";

   procedure vgmstream_set_log_stdout (level : int)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:89
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_set_log_stdout";

  --possible future public/opaque API
  -- opaque player state  
  --#define VGMSTREAM_CTX_VERSION 1
  -- Setups base vgmstream player context.  
  -- Sets default config, that will be applied to song on open (some formats like TXTP may override
  -- * these settings).
  -- * May only be called without song loaded (before _open or after _close), otherwise ignored.   

  -- Opens a new STREAMFILE to play. Returns < 0 on error when the file isn't recogniced.
  -- * If file has subsongs, first open usually loads first subsong. get_info then can be used to check
  -- * whether file has more subsongs (total_subsongs > 1), and call others.
  -- *   

  -- file's samples (not final duration)  
  -- 0=not set, N=loaded subsong N  
  -- 0=format has no subsongs, N=has N subsongs  
  -- file's average bitrate  
  --const int codec_bitrate;      /* codec's average bitrate  
  -- descriptions  
  --const char* codec;
  --const char* layout;
  --const char* metadata;
  --int type;                     /* 0=pcm16, 1=float32, always interleaved: [0]=ch0, [1]=ch1 ...  
  -- Get info from current song.  
  -- Gets final time based on config and current song. If config is set to "play forever"
  -- * this still returns final time based on config as a reference. Returns > 0 on success.  

  -- Gets current position within song. When "play forever" is set, it'll clamp results to total_time.  
  -- Seeks to position  
  -- Closes current song.  
  -- Frees vgmstream context.  
  -- Converts samples. returns number of rendered samples, or <=0 if no more
  -- * samples left (will fill buffer with silence)  

  -- ******************************************  
  -- TAGS: loads key=val tags from a file        
  -- ******************************************  
  -- opaque tag state  
   type VGMSTREAM_TAGS is null record;   -- incomplete struct

  -- Initializes TAGS and returns pointers to extracted strings (always valid but change
  -- * on every vgmstream_tags_next_tag call). Next functions are safe to call even if this fails (validate NULL).
  -- * ex.: const char *tag_key, *tag_val; tags=vgmstream_tags_init(&tag_key, &tag_val);  

   function vgmstream_tags_init (tag_key : System.Address; tag_val : System.Address) return access VGMSTREAM_TAGS  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:202
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_tags_init";

  -- Resets tagfile to restart reading from the beginning for a new filename.
  -- * Must be called first before extracting tags.  

   procedure vgmstream_tags_reset (tags : access VGMSTREAM_TAGS; target_filename : Interfaces.C.Strings.chars_ptr)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:206
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_tags_reset";

  -- Extracts next valid tag in tagfile to *tag. Returns 0 if no more tags are found (meant to be
  -- * called repeatedly until 0). Key/values are trimmed and values can be in UTF-8.  

   function vgmstream_tags_next_tag (tags : access VGMSTREAM_TAGS; tagfile : access streamfile_h.STREAMFILE) return int  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_tags_next_tag";

  -- Closes tag file  
   procedure vgmstream_tags_close (tags : access VGMSTREAM_TAGS)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:214
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_tags_close";

  -- ******************************************  
  -- MIXING: modifies vgmstream output           
  -- ******************************************  
  -- Enables mixing effects, with max outbuf samples as a hint. Once active, plugin
  -- * must use returned input_channels to create outbuf and output_channels to output audio.
  -- * max_sample_count may be 0 if you only need to query values and not actually enable it.
  -- * Needs to be enabled last after adding effects.  

   procedure vgmstream_mixing_enable
     (the_vgmstream : access vgmstream_h.VGMSTREAM;
      max_sample_count : bits_stdint_intn_h.int32_t;
      input_channels : access int;
      output_channels : access int)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:225
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_mixing_enable";

  -- sets automatic downmixing if vgmstream's channels are higher than max_channels  
   procedure vgmstream_mixing_autodownmix (the_vgmstream : access vgmstream_h.VGMSTREAM; max_channels : int)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:228
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_mixing_autodownmix";

  -- downmixes to get stereo from start channel  
   procedure vgmstream_mixing_stereo_only (the_vgmstream : access vgmstream_h.VGMSTREAM; start : int)  -- /home/andrew/src/vgmstream-r1721/src/plugins.h:231
   with Import => True, 
        Convention => C, 
        External_Name => "vgmstream_mixing_stereo_only";

  -- sets a fadeout  
  --void vgmstream_mixing_fadeout(VGMSTREAM *vgmstream, float start_second, float duration_seconds);
end plugins_h;
