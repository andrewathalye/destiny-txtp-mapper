pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with pulse_context_h;
with bits_stdint_uintn_h;
with System;
limited with pulse_stream_h;
with stddef_h;
with Interfaces.C.Strings;
limited with pulse_operation_h;
with pulse_volume_h;
limited with pulse_proplist_h;

package pulse_scache_h is

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2004-2006 Lennart Poettering
  --  Copyright 2006 Pierre Ossman <ossman@cendio.se> for Cendio AB
  --  PulseAudio is free software; you can redistribute it and/or modify
  --  it under the terms of the GNU Lesser General Public License as published
  --  by the Free Software Foundation; either version 2.1 of the License,
  --  or (at your option) any later version.
  --  PulseAudio is distributed in the hope that it will be useful, but
  --  WITHOUT ANY WARRANTY; without even the implied warranty of
  --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  --  General Public License for more details.
  --  You should have received a copy of the GNU Lesser General Public License
  --  along with PulseAudio; if not, see <http://www.gnu.org/licenses/>.
  --** 

  --* \page scache Sample Cache
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * The sample cache provides a simple way of overcoming high network latencies
  -- * and reducing bandwidth. Instead of streaming a sound precisely when it
  -- * should be played, it is stored on the server and only the command to start
  -- * playing it needs to be sent.
  -- *
  -- * \section create_sec Creation
  -- *
  -- * To create a sample, the normal stream API is used (see \ref streams). The
  -- * function pa_stream_connect_upload() will make sure the stream is stored as
  -- * a sample on the server.
  -- *
  -- * To complete the upload, pa_stream_finish_upload() is called and the sample
  -- * will receive the same name as the stream. If the upload should be aborted,
  -- * simply call pa_stream_disconnect().
  -- *
  -- * \section play_sec Playing samples
  -- *
  -- * To play back a sample, simply call pa_context_play_sample():
  -- *
  -- * \code
  -- * pa_operation *o;
  -- *
  -- * o = pa_context_play_sample(my_context,
  -- *                            "sample2",       // Name of my sample
  -- *                            NULL,            // Use default sink
  -- *                            PA_VOLUME_NORM,  // Full volume
  -- *                            NULL,            // Don't need a callback
  -- *                            NULL
  -- *                            );
  -- * if (o)
  -- *     pa_operation_unref(o);
  -- * \endcode
  -- *
  -- * \section rem_sec Removing samples
  -- *
  -- * When a sample is no longer needed, it should be removed on the server to
  -- * save resources. The sample is deleted using pa_context_remove_sample().
  --  

  --* \file
  -- * All sample cache related routines
  -- *
  -- * See also \subpage scache
  --  

  --* Callback prototype for pa_context_play_sample_with_proplist(). The
  -- * idx value is the index of the sink input object, or
  -- * PA_INVALID_INDEX on failure. \since 0.9.11  

   type pa_context_play_sample_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : bits_stdint_uintn_h.uint32_t;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/scache.h:85

  --* Make this stream a sample upload stream. Returns zero on success.  
   function pa_stream_connect_upload (s : access pulse_stream_h.pa_stream; length : stddef_h.size_t) return int  -- /usr/include/pulse/scache.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "pa_stream_connect_upload";

  --* Finish the sample upload, the stream name will become the sample
  -- * name. You cancel a sample upload by issuing
  -- * pa_stream_disconnect(). Returns zero on success.  

   function pa_stream_finish_upload (s : access pulse_stream_h.pa_stream) return int  -- /usr/include/pulse/scache.h:93
   with Import => True, 
        Convention => C, 
        External_Name => "pa_stream_finish_upload";

  --* Remove a sample from the sample cache. Returns an operation object which
  -- * may be used to cancel the operation while it is running.  

   function pa_context_remove_sample
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/scache.h:97
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_remove_sample";

  --* Play a sample from the sample cache to the specified device. If
  -- * the latter is NULL use the default sink. Returns an operation
  -- * object  

   function pa_context_play_sample
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      dev : Interfaces.C.Strings.chars_ptr;
      volume : pulse_volume_h.pa_volume_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/scache.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_play_sample";

  --*< Context  
  --*< Name of the sample to play  
  --*< Sink to play this sample on  
  --*< Volume to play this sample with. Starting with 0.9.15 you may pass here PA_VOLUME_INVALID which will leave the decision about the volume to the server side, which is a good idea.  
  --*< Call this function after successfully starting playback, or NULL  
  --*< Userdata to pass to the callback  
  --* Play a sample from the sample cache to the specified device,
  -- * allowing specification of a property list for the playback
  -- * stream. If the latter is NULL use the default sink. Returns an
  -- * operation object. \since 0.9.11  

   function pa_context_play_sample_with_proplist
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      dev : Interfaces.C.Strings.chars_ptr;
      volume : pulse_volume_h.pa_volume_t;
      proplist : access constant pulse_proplist_h.pa_proplist;
      cb : pa_context_play_sample_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/scache.h:114
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_play_sample_with_proplist";

  --*< Context  
  --*< Name of the sample to play  
  --*< Sink to play this sample on  
  --*< Volume to play this sample with. Starting with 0.9.15 you may pass here PA_VOLUME_INVALID which will leave the decision about the volume to the server side, which is a good idea.   
  --*< Property list for this sound. The property list of the cached entry will have this merged into it.  
  --*< Call this function after successfully starting playback, or NULL  
  --*< Userdata to pass to the callback  
end pulse_scache_h;
