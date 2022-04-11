pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with pulse_sample_h;

package pulse_rtclock_h is

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2004-2009 Lennart Poettering
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
  -- *  Monotonic clock utilities.  

  --* Return the current monotonic system time in usec, if such a clock
  -- * is available.  If it is not available this will return the
  -- * wallclock time instead.  \since 0.9.16  

   function pa_rtclock_now return pulse_sample_h.pa_usec_t  -- /usr/include/pulse/rtclock.h:34
   with Import => True, 
        Convention => C, 
        External_Name => "pa_rtclock_now";

end pulse_rtclock_h;
