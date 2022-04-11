pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with pulse_mainloop_api_h;
with System;

package pulse_mainloop_signal_h is

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2004-2008 Lennart Poettering
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

  --* \file
  -- * UNIX signal support for main loops. In contrast to other
  -- * main loop event sources such as timer and IO events, UNIX signal
  -- * support requires modification of the global process
  -- * environment. Due to this the generic main loop abstraction layer as
  -- * defined in \ref mainloop-api.h doesn't have direct support for UNIX
  -- * signals. However, you may hook signal support into an abstract main loop via the routines defined herein.
  --  

  --* An opaque UNIX signal event source object  
   type pa_signal_event is null record;   -- incomplete struct

  --* Callback prototype for signal events  
   type pa_signal_cb_t is access procedure
        (arg1 : access pulse_mainloop_api_h.pa_mainloop_api;
         arg2 : access pa_signal_event;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/mainloop-signal.h:42

  --* Destroy callback prototype for signal events  
   type pa_signal_destroy_cb_t is access procedure
        (arg1 : access pulse_mainloop_api_h.pa_mainloop_api;
         arg2 : access pa_signal_event;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/mainloop-signal.h:45

  --* Initialize the UNIX signal subsystem and bind it to the specified main loop  
   function pa_signal_init (api : access pulse_mainloop_api_h.pa_mainloop_api) return int  -- /usr/include/pulse/mainloop-signal.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "pa_signal_init";

  --* Cleanup the signal subsystem  
   procedure pa_signal_done  -- /usr/include/pulse/mainloop-signal.h:51
   with Import => True, 
        Convention => C, 
        External_Name => "pa_signal_done";

  --* Create a new UNIX signal event source object  
   function pa_signal_new
     (sig : int;
      callback : pa_signal_cb_t;
      userdata : System.Address) return access pa_signal_event  -- /usr/include/pulse/mainloop-signal.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "pa_signal_new";

  --* Free a UNIX signal event source object  
   procedure pa_signal_free (e : access pa_signal_event)  -- /usr/include/pulse/mainloop-signal.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "pa_signal_free";

  --* Set a function that is called when the signal event source is destroyed. Use this to free the userdata argument if required  
   procedure pa_signal_set_destroy (e : access pa_signal_event; callback : pa_signal_destroy_cb_t)  -- /usr/include/pulse/mainloop-signal.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "pa_signal_set_destroy";

end pulse_mainloop_signal_h;
