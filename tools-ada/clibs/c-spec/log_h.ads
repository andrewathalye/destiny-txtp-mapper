pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;

package log_h is

   --  unsupported macro: GNUC_LOG_ATRIB __attribute__ ((format(printf, 1, 2)))
   --  unsupported macro: GNUC_ASR_ATRIB __attribute__ ((format(printf, 2, 3)))
  -- Dumb logger utils (tuned for simplicity). Notes:
  -- * - must set callback/defaults to print anything
  -- * - mainly used to print info for users, like format detected but wrong size
  -- *   (don't clutter by logging code that happens most of the time, since log may be shared with other plugins)
  -- * - callbacks receive formed string for simplicity (to be adjusted)
  -- * - DO NOT put logs in tight loops (like decoders), slow fn calls and clutter
  -- *   (metas are usually fine but also consider cluttering)
  -- * - as compiler must support variable args, needs to pass VGM_LOG_OUTPUT flag to enable
  -- * - debug logs are removed unless VGM_DEBUG_OUTPUT is passed to compiler args
  -- *   (passing either of them works)
  -- * - callback should be thread-safe (no internal checks and only one log ATM)
  -- * - still WIP, some stuff not working ATM or may change
  --  

  -- compiler hints to force printf-style checks, butt-ugly but so useful...  
  -- supposedly MSCV has _Printf_format_string_ with /analyze but I can't get it to work  
  -- void (*callback)(int level, const char* str);
   procedure vgm_log_set_callback
     (ctx_p : System.Address;
      level : int;
      c_type : int;
      callback : System.Address)  -- /home/andrew/src/vgmstream-r1721/src/util/log.h:29
   with Import => True, 
        Convention => C, 
        External_Name => "vgm_log_set_callback";

  --void* ctx, 
  --void* ctx, 
  --void vgm_logi_once(/*void* ctx, int* once_flag, */ const char* fmt, ...);
  --void* ctx, 
  -- original stdout logging for debugging and regression testing purposes, may be removed later.
  -- * Needs C99 variadic macros, uses do..while to force ";" as statement  

  -- prints to a file  
  -- prints a buffer/array  
end log_h;
