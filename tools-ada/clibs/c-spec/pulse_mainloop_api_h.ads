pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;
limited with bits_types_struct_timeval_h;

package pulse_mainloop_api_h is

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2004-2006 Lennart Poettering
  --  Copyright 2006 Pierre Ossman <ossman@cendio.se> for Cendio AB
  --  PulseAudio is free software; you can redistribute it and/or modify
  --  it under the terms of the GNU Lesser General Public License as
  --  published by the Free Software Foundation; either version 2.1 of the
  --  License, or (at your option) any later version.
  --  PulseAudio is distributed in the hope that it will be useful, but
  --  WITHOUT ANY WARRANTY; without even the implied warranty of
  --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  --  Lesser General Public License for more details.
  --  You should have received a copy of the GNU Lesser General Public
  --  License along with PulseAudio; if not, see <http://www.gnu.org/licenses/>.
  --** 

  --* \file
  -- *
  -- * Main loop abstraction layer. Both the PulseAudio core and the
  -- * PulseAudio client library use a main loop abstraction layer. Due to
  -- * this it is possible to embed PulseAudio into other
  -- * applications easily. Three main loop implementations are
  -- * currently available:
  -- * \li A minimal implementation based on the C library's poll() function
  -- *     (See \ref mainloop.h).
  -- * \li A special version of the previous implementation where all of
  -- *     PulseAudio's internal handling runs in a separate thread
  -- *     (See \ref thread-mainloop.h).
  -- * \li A wrapper around the GLIB main loop. Use this to embed PulseAudio into
  -- *     your GLIB/GTK+/GNOME programs (See \ref glib-mainloop.h).
  -- *
  -- * The structure pa_mainloop_api is used as a vtable for the main loop abstraction.
  -- *
  -- * This mainloop abstraction layer has no direct support for UNIX signals.
  -- * Generic, mainloop implementation agnostic support is available through
  -- * \ref mainloop-signal.h.
  -- *  

  --* An abstract mainloop API vtable  
   type pa_mainloop_api;
  --* A bitmask for IO events  
   subtype pa_io_event_flags is unsigned;
   PA_IO_EVENT_NULL : constant pa_io_event_flags := 0;
   PA_IO_EVENT_INPUT : constant pa_io_event_flags := 1;
   PA_IO_EVENT_OUTPUT : constant pa_io_event_flags := 2;
   PA_IO_EVENT_HANGUP : constant pa_io_event_flags := 4;
   PA_IO_EVENT_ERROR : constant pa_io_event_flags := 8;  -- /usr/include/pulse/mainloop-api.h:57

  --*< No event  
  --*< Input event  
  --*< Output event  
  --*< Hangup event  
  --*< Error event  
   subtype pa_io_event_flags_t is pa_io_event_flags;  -- /usr/include/pulse/mainloop-api.h:63

  --* An opaque IO event source object  
   type pa_io_event is null record;   -- incomplete struct

  --* An IO event callback prototype \since 0.9.3  
   type pa_io_event_cb_t is access procedure
        (arg1 : access pa_mainloop_api;
         arg2 : access pa_io_event;
         arg3 : int;
         arg4 : pa_io_event_flags_t;
         arg5 : System.Address)
   with Convention => C;  -- /usr/include/pulse/mainloop-api.h:68

  --* A IO event destroy callback prototype \since 0.9.3  
   type pa_io_event_destroy_cb_t is access procedure
        (arg1 : access pa_mainloop_api;
         arg2 : access pa_io_event;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/mainloop-api.h:70

  --* An opaque timer event source object  
   type pa_time_event is null record;   -- incomplete struct

  --* A time event callback prototype \since 0.9.3  
   type pa_time_event_cb_t is access procedure
        (arg1 : access pa_mainloop_api;
         arg2 : access pa_time_event;
         arg3 : access constant bits_types_struct_timeval_h.timeval;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/mainloop-api.h:75

  --* A time event destroy callback prototype \since 0.9.3  
   type pa_time_event_destroy_cb_t is access procedure
        (arg1 : access pa_mainloop_api;
         arg2 : access pa_time_event;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/mainloop-api.h:77

  --* An opaque deferred event source object. Events of this type are triggered once in every main loop iteration  
   type pa_defer_event is null record;   -- incomplete struct

  --* A defer event callback prototype \since 0.9.3  
   type pa_defer_event_cb_t is access procedure
        (arg1 : access pa_mainloop_api;
         arg2 : access pa_defer_event;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/mainloop-api.h:82

  --* A defer event destroy callback prototype \since 0.9.3  
   type pa_defer_event_destroy_cb_t is access procedure
        (arg1 : access pa_mainloop_api;
         arg2 : access pa_defer_event;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/mainloop-api.h:84

  --* An abstract mainloop API vtable  
  --* A pointer to some private, arbitrary data of the main loop implementation  
   type pa_mainloop_api is record
      userdata : System.Address;  -- /usr/include/pulse/mainloop-api.h:89
      io_new : access function
           (arg1 : access pa_mainloop_api;
            arg2 : int;
            arg3 : pa_io_event_flags_t;
            arg4 : pa_io_event_cb_t;
            arg5 : System.Address) return access pa_io_event;  -- /usr/include/pulse/mainloop-api.h:92
      io_enable : access procedure (arg1 : access pa_io_event; arg2 : pa_io_event_flags_t);  -- /usr/include/pulse/mainloop-api.h:94
      io_free : access procedure (arg1 : access pa_io_event);  -- /usr/include/pulse/mainloop-api.h:96
      io_set_destroy : access procedure (arg1 : access pa_io_event; arg2 : pa_io_event_destroy_cb_t);  -- /usr/include/pulse/mainloop-api.h:98
      time_new : access function
           (arg1 : access pa_mainloop_api;
            arg2 : access constant bits_types_struct_timeval_h.timeval;
            arg3 : pa_time_event_cb_t;
            arg4 : System.Address) return access pa_time_event;  -- /usr/include/pulse/mainloop-api.h:101
      time_restart : access procedure (arg1 : access pa_time_event; arg2 : access constant bits_types_struct_timeval_h.timeval);  -- /usr/include/pulse/mainloop-api.h:103
      time_free : access procedure (arg1 : access pa_time_event);  -- /usr/include/pulse/mainloop-api.h:105
      time_set_destroy : access procedure (arg1 : access pa_time_event; arg2 : pa_time_event_destroy_cb_t);  -- /usr/include/pulse/mainloop-api.h:107
      defer_new : access function
           (arg1 : access pa_mainloop_api;
            arg2 : pa_defer_event_cb_t;
            arg3 : System.Address) return access pa_defer_event;  -- /usr/include/pulse/mainloop-api.h:110
      defer_enable : access procedure (arg1 : access pa_defer_event; arg2 : int);  -- /usr/include/pulse/mainloop-api.h:112
      defer_free : access procedure (arg1 : access pa_defer_event);  -- /usr/include/pulse/mainloop-api.h:114
      defer_set_destroy : access procedure (arg1 : access pa_defer_event; arg2 : pa_defer_event_destroy_cb_t);  -- /usr/include/pulse/mainloop-api.h:116
      quit : access procedure (arg1 : access pa_mainloop_api; arg2 : int);  -- /usr/include/pulse/mainloop-api.h:119
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/mainloop-api.h:87

  --* Create a new IO event source object  
  --* Enable or disable IO events on this object  
  --* Free a IO event source object  
  --* Set a function that is called when the IO event source is destroyed. Use this to free the userdata argument if required  
  --* Create a new timer event source object for the specified Unix time  
  --* Restart a running or expired timer event source with a new Unix time  
  --* Free a deferred timer event source object  
  --* Set a function that is called when the timer event source is destroyed. Use this to free the userdata argument if required  
  --* Create a new deferred event source object  
  --* Enable or disable a deferred event source temporarily  
  --* Free a deferred event source object  
  --* Set a function that is called when the deferred event source is destroyed. Use this to free the userdata argument if required  
  --* Exit the main loop and return the specified retval 
  --* Run the specified callback function once from the main loop using an
  -- * anonymous defer event. If the mainloop runs in a different thread, you need
  -- * to follow the mainloop implementation's rules regarding how to safely create
  -- * defer events. In particular, if you're using \ref pa_threaded_mainloop, you
  -- * must lock the mainloop before calling this function.  

   procedure pa_mainloop_api_once
     (m : access pa_mainloop_api;
      callback : access procedure (arg1 : access pa_mainloop_api; arg2 : System.Address);
      userdata : System.Address)  -- /usr/include/pulse/mainloop-api.h:127
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_api_once";

end pulse_mainloop_api_h;
