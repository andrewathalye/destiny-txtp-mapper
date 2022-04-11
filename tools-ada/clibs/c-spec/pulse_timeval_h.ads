pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with bits_types_struct_timeval_h;
with pulse_sample_h;

package pulse_timeval_h is

   --  unsupported macro: PA_MSEC_PER_SEC ((pa_usec_t) 1000ULL)
   --  unsupported macro: PA_USEC_PER_SEC ((pa_usec_t) 1000000ULL)
   --  unsupported macro: PA_NSEC_PER_SEC ((unsigned long long) 1000000000ULL)
   --  unsupported macro: PA_USEC_PER_MSEC ((pa_usec_t) 1000ULL)
   --  unsupported macro: PA_NSEC_PER_MSEC ((unsigned long long) 1000000ULL)
   --  unsupported macro: PA_NSEC_PER_USEC ((unsigned long long) 1000ULL)
   --  unsupported macro: PA_USEC_INVALID ((pa_usec_t) -1)
   --  unsupported macro: PA_USEC_MAX ((pa_usec_t) -2)
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
  -- * Utility functions for handling timeval calculations  

  --* The number of milliseconds in a second  
  --* The number of microseconds in a second  
  --* The number of nanoseconds in a second  
  --* The number of microseconds in a millisecond  
  --* The number of nanoseconds in a millisecond  
  --* The number of nanoseconds in a microsecond  
  --* Invalid time in usec. \since 0.9.15  
  --* Biggest time in usec. \since 0.9.18  
  --* Return the current wallclock timestamp, just like UNIX gettimeofday().  
   function pa_gettimeofday (tv : access bits_types_struct_timeval_h.timeval) return access bits_types_struct_timeval_h.timeval  -- /usr/include/pulse/timeval.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "pa_gettimeofday";

  --* Calculate the difference between the two specified timeval
  -- * structs.  

   function pa_timeval_diff (a : access constant bits_types_struct_timeval_h.timeval; b : access constant bits_types_struct_timeval_h.timeval) return pulse_sample_h.pa_usec_t  -- /usr/include/pulse/timeval.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "pa_timeval_diff";

  --* Compare the two timeval structs and return 0 when equal, negative when a < b, positive otherwise  
   function pa_timeval_cmp (a : access constant bits_types_struct_timeval_h.timeval; b : access constant bits_types_struct_timeval_h.timeval) return int  -- /usr/include/pulse/timeval.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "pa_timeval_cmp";

  --* Return the time difference between now and the specified timestamp  
   function pa_timeval_age (tv : access constant bits_types_struct_timeval_h.timeval) return pulse_sample_h.pa_usec_t  -- /usr/include/pulse/timeval.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "pa_timeval_age";

  --* Add the specified time in microseconds to the specified timeval structure  
   function pa_timeval_add (tv : access bits_types_struct_timeval_h.timeval; v : pulse_sample_h.pa_usec_t) return access bits_types_struct_timeval_h.timeval  -- /usr/include/pulse/timeval.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "pa_timeval_add";

  --* Subtract the specified time in microseconds to the specified timeval structure. \since 0.9.11  
   function pa_timeval_sub (tv : access bits_types_struct_timeval_h.timeval; v : pulse_sample_h.pa_usec_t) return access bits_types_struct_timeval_h.timeval  -- /usr/include/pulse/timeval.h:77
   with Import => True, 
        Convention => C, 
        External_Name => "pa_timeval_sub";

  --* Store the specified usec value in the timeval struct. \since 0.9.7  
   function pa_timeval_store (tv : access bits_types_struct_timeval_h.timeval; v : pulse_sample_h.pa_usec_t) return access bits_types_struct_timeval_h.timeval  -- /usr/include/pulse/timeval.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "pa_timeval_store";

  --* Load the specified tv value and return it in usec. \since 0.9.7  
   function pa_timeval_load (tv : access constant bits_types_struct_timeval_h.timeval) return pulse_sample_h.pa_usec_t  -- /usr/include/pulse/timeval.h:83
   with Import => True, 
        Convention => C, 
        External_Name => "pa_timeval_load";

end pulse_timeval_h;
