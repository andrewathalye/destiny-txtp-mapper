pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with stddef_h;

package pulse_util_h is

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
  -- * Assorted utility functions  

  --* Return the current username in the specified string buffer.  
   function pa_get_user_name (s : Interfaces.C.Strings.chars_ptr; l : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/util.h:35
   with Import => True, 
        Convention => C, 
        External_Name => "pa_get_user_name";

  --* Return the current hostname in the specified buffer.  
   function pa_get_host_name (s : Interfaces.C.Strings.chars_ptr; l : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/util.h:38
   with Import => True, 
        Convention => C, 
        External_Name => "pa_get_host_name";

  --* Return the fully qualified domain name in s  
   function pa_get_fqdn (s : Interfaces.C.Strings.chars_ptr; l : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/util.h:41
   with Import => True, 
        Convention => C, 
        External_Name => "pa_get_fqdn";

  --* Return the home directory of the current user  
   function pa_get_home_dir (s : Interfaces.C.Strings.chars_ptr; l : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/util.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "pa_get_home_dir";

  --* Return the binary file name of the current process. This is not
  -- * supported on all architectures, in which case NULL is returned.  

   function pa_get_binary_name (s : Interfaces.C.Strings.chars_ptr; l : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/util.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "pa_get_binary_name";

  --* Return a pointer to the filename inside a path (which is the last
  -- * component). If passed NULL will return NULL.  

   function pa_path_get_filename (p : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/util.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "pa_path_get_filename";

  --* Wait t milliseconds  
   function pa_msleep (t : unsigned_long) return int  -- /usr/include/pulse/util.h:55
   with Import => True, 
        Convention => C, 
        External_Name => "pa_msleep";

  --* Make the calling thread realtime if we can. On Linux, this uses RealTimeKit
  -- * if available and POSIX APIs otherwise (the latter applies to other UNIX
  -- * variants as well). This is also implemented for macOS and Windows.
  -- * \since 13.0  

   function pa_thread_make_realtime (rtprio : int) return int  -- /usr/include/pulse/util.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "pa_thread_make_realtime";

end pulse_util_h;
