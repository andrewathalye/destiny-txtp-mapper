pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;
with pulse_def_h;

package pulse_operation_h is

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2004-2006 Lennart Poettering
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
  -- * Asynchronous operations  

  --* An asynchronous operation object  
   type pa_operation is null record;   -- incomplete struct

  --* A callback for operation state changes  
   type pa_operation_notify_cb_t is access procedure (arg1 : access pa_operation; arg2 : System.Address)
   with Convention => C;  -- /usr/include/pulse/operation.h:36

  --* Increase the reference count by one  
   function pa_operation_ref (o : access pa_operation) return access pa_operation  -- /usr/include/pulse/operation.h:39
   with Import => True, 
        Convention => C, 
        External_Name => "pa_operation_ref";

  --* Decrease the reference count by one  
   procedure pa_operation_unref (o : access pa_operation)  -- /usr/include/pulse/operation.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "pa_operation_unref";

  --* Cancel the operation. Beware! This will not necessarily cancel the
  -- * execution of the operation on the server side. However it will make
  -- * sure that the callback associated with this operation will not be
  -- * called anymore, effectively disabling the operation from the client
  -- * side's view.  

   procedure pa_operation_cancel (o : access pa_operation)  -- /usr/include/pulse/operation.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "pa_operation_cancel";

  --* Return the current status of the operation  
   function pa_operation_get_state (o : access constant pa_operation) return pulse_def_h.pa_operation_state_t  -- /usr/include/pulse/operation.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "pa_operation_get_state";

  --* Set the callback function that is called when the operation state
  -- * changes. Usually this is not necessary, since the functions that
  -- * create pa_operation objects already take a callback that is called
  -- * when the operation finishes. Registering a state change callback is
  -- * mainly useful, if you want to get called back also if the operation
  -- * gets cancelled. \since 4.0  

   procedure pa_operation_set_state_callback
     (o : access pa_operation;
      cb : pa_operation_notify_cb_t;
      userdata : System.Address)  -- /usr/include/pulse/operation.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "pa_operation_set_state_callback";

end pulse_operation_h;
