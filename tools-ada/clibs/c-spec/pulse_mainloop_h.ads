pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with pulse_mainloop_api_h;
with System;

package pulse_mainloop_h is

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

   type pollfd is null record;   -- incomplete struct

  --* \page mainloop Main Loop
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * The built-in main loop implementation is based on the poll() system call.
  -- * It supports the functions defined in the main loop abstraction and very
  -- * little else.
  -- *
  -- * The main loop is created using pa_mainloop_new() and destroyed using
  -- * pa_mainloop_free(). To get access to the main loop abstraction,
  -- * pa_mainloop_get_api() is used.
  -- *
  -- * \section iter_sec Iteration
  -- *
  -- * The main loop is designed around the concept of iterations. Each iteration
  -- * consists of three steps that repeat during the application's entire
  -- * lifetime:
  -- *
  -- * -# Prepare - Build a list of file descriptors
  -- *               that need to be monitored and calculate the next timeout.
  -- * -# Poll - Execute the actual poll() system call.
  -- * -# Dispatch - Dispatch any events that have fired.
  -- *
  -- * When using the main loop, the application can either execute each
  -- * iteration, one at a time, using pa_mainloop_iterate(), or let the library
  -- * iterate automatically using pa_mainloop_run().
  -- *
  -- * \section thread_sec Threads
  -- *
  -- * The main loop functions are designed to be thread safe, but the objects
  -- * are not. What this means is that multiple main loops can be used, but only
  -- * one object per thread.
  -- *
  --  

  --* \file
  -- *
  -- * A minimal main loop implementation based on the C library's poll()
  -- * function. Using the routines defined herein you may create a simple
  -- * main loop supporting the generic main loop abstraction layer as
  -- * defined in \ref mainloop-api.h. This implementation is thread safe
  -- * as long as you access the main loop object from a single thread only.
  -- *
  -- * See also \subpage mainloop
  --  

  --* An opaque main loop object  
   type pa_mainloop is null record;   -- incomplete struct

  --* Allocate a new main loop object. Free with pa_mainloop_free.  
   function pa_mainloop_new return access pa_mainloop  -- /usr/include/pulse/mainloop.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_new";

  --* Free a main loop object  
   procedure pa_mainloop_free (m : access pa_mainloop)  -- /usr/include/pulse/mainloop.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_free";

  --* Prepare for a single iteration of the main loop. Returns a negative value
  --on error or exit request. timeout specifies a maximum timeout for the subsequent
  --poll, or -1 for blocking behaviour. The timeout is specified in microseconds.  

   function pa_mainloop_prepare (m : access pa_mainloop; timeout : int) return int  -- /usr/include/pulse/mainloop.h:89
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_prepare";

  --* Execute the previously prepared poll. Returns a negative value on error. 
   function pa_mainloop_poll (m : access pa_mainloop) return int  -- /usr/include/pulse/mainloop.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_poll";

  --* Dispatch timeout, io and deferred events from the previously executed poll. Returns
  --a negative value on error. On success returns the number of source dispatched.  

   function pa_mainloop_dispatch (m : access pa_mainloop) return int  -- /usr/include/pulse/mainloop.h:96
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_dispatch";

  --* Return the return value as specified with the main loop's quit() routine.  
   function pa_mainloop_get_retval (m : access constant pa_mainloop) return int  -- /usr/include/pulse/mainloop.h:99
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_get_retval";

  --* Run a single iteration of the main loop. This is a convenience function
  --for pa_mainloop_prepare(), pa_mainloop_poll() and pa_mainloop_dispatch().
  --Returns a negative value on error or exit request. If block is nonzero,
  --block for events if none are queued. Optionally return the return value as
  --specified with the main loop's quit() routine in the integer variable retval points
  --to. On success returns the number of sources dispatched in this iteration.  

   function pa_mainloop_iterate
     (m : access pa_mainloop;
      block : int;
      retval : access int) return int  -- /usr/include/pulse/mainloop.h:107
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_iterate";

  --* Run unlimited iterations of the main loop object until the main loop's
  --quit() routine is called. Returns a negative value on error. Optionally return
  --the return value as specified with the main loop's quit() routine in the integer
  --variable retval points to.  

   function pa_mainloop_run (m : access pa_mainloop; retval : access int) return int  -- /usr/include/pulse/mainloop.h:113
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_run";

  --* Return the abstract main loop abstraction layer vtable for this
  --    main loop. No need to free the API as it is owned by the loop
  --    and is destroyed when the loop is freed.  

   function pa_mainloop_get_api (m : access pa_mainloop) return access pulse_mainloop_api_h.pa_mainloop_api  -- /usr/include/pulse/mainloop.h:118
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_get_api";

  --* Shutdown the main loop with the specified return value  
   procedure pa_mainloop_quit (m : access pa_mainloop; retval : int)  -- /usr/include/pulse/mainloop.h:121
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_quit";

  --* Interrupt a running poll (for threaded systems)  
   procedure pa_mainloop_wakeup (m : access pa_mainloop)  -- /usr/include/pulse/mainloop.h:124
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_wakeup";

  --* Generic prototype of a poll() like function  
   type pa_poll_func is access function
        (arg1 : access pollfd;
         arg2 : unsigned_long;
         arg3 : int;
         arg4 : System.Address) return int
   with Convention => C;  -- /usr/include/pulse/mainloop.h:127

  --* Change the poll() implementation  
   procedure pa_mainloop_set_poll_func
     (m : access pa_mainloop;
      poll_func : pa_poll_func;
      userdata : System.Address)  -- /usr/include/pulse/mainloop.h:130
   with Import => True, 
        Convention => C, 
        External_Name => "pa_mainloop_set_poll_func";

end pulse_mainloop_h;
