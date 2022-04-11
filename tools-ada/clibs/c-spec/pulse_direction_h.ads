pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with pulse_def_h;
with Interfaces.C.Strings;

package pulse_direction_h is

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2014 Intel Corporation
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
  -- * Utility functions for \ref pa_direction_t.  

  --* Return non-zero if the given value is a valid direction (either input,
  -- * output or bidirectional). \since 6.0  

   function pa_direction_valid (direction : pulse_def_h.pa_direction_t) return int  -- /usr/include/pulse/direction.h:30
   with Import => True, 
        Convention => C, 
        External_Name => "pa_direction_valid";

  --* Return a textual representation of the direction. \since 6.0  
   function pa_direction_to_string (direction : pulse_def_h.pa_direction_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/direction.h:33
   with Import => True, 
        Convention => C, 
        External_Name => "pa_direction_to_string";

end pulse_direction_h;
