pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with pulse_context_h;
with pulse_def_h;
with bits_stdint_uintn_h;
with System;
limited with pulse_operation_h;

package pulse_subscribe_h is

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

  --* \page subscribe Event Subscription
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * The application can be notified, asynchronously, whenever the internal
  -- * layout of the server changes. Possible notifications are described in the
  -- * \ref pa_subscription_event_type and \ref pa_subscription_mask
  -- * enumerations.
  -- *
  -- * The application sets the notification mask using pa_context_subscribe()
  -- * and the function that will be called whenever a notification occurs using
  -- * pa_context_set_subscribe_callback().
  -- *
  -- * The callback will be called with a \ref pa_subscription_event_type_t
  -- * representing the event that caused the callback. Clients can examine what
  -- * object changed using \ref PA_SUBSCRIPTION_EVENT_FACILITY_MASK. The actual
  -- * event type can then be extracted with \ref PA_SUBSCRIPTION_EVENT_TYPE_MASK.
  -- * Please note that the masked values are integers, not flags (so you will
  -- * check the object/event type using a comparison not a binary AND). For
  -- * example, the callback might look something like:
  -- *
  --@verbatim
  --void my_subscription_callback(pa_context *c, pa_subscription_event_type_t t,
  --                              uint32_t idx, void *userdata) {
  --    if ((t & PA_SUBSCRIPTION_EVENT_FACILITY_MASK) == PA_SUBSCRIPTION_EVENT_SOURCE) {
  --        if ((t & PA_SUBSCRIPTION_EVENT_TYPE_MASK) == PA_SUBSCRIPTION_EVENT_NEW) {
  --            ... a source was added, let's do stuff! ...
  --        }
  --    }
  --}
  --@endverbatim
  --  

  --* \file
  -- * Daemon introspection event subscription subsystem.
  -- *
  -- * See also \subpage subscribe
  --  

  --* Subscription event callback prototype  
   type pa_context_subscribe_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : pulse_def_h.pa_subscription_event_type_t;
         arg3 : bits_stdint_uintn_h.uint32_t;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/subscribe.h:73

  --* Enable event notification  
   function pa_context_subscribe
     (c : access pulse_context_h.pa_context;
      m : pulse_def_h.pa_subscription_mask_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/subscribe.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_subscribe";

  --* Set the context specific call back function that is called whenever the state of the daemon changes  
   procedure pa_context_set_subscribe_callback
     (c : access pulse_context_h.pa_context;
      cb : pa_context_subscribe_cb_t;
      userdata : System.Address)  -- /usr/include/pulse/subscribe.h:79
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_subscribe_callback";

end pulse_subscribe_h;
