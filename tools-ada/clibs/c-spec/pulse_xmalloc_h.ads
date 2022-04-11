pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with stddef_h;
with System;
with Interfaces.C.Strings;

package pulse_xmalloc_h is

   --  arg-macro: function pa_xnew (type, n)
   --    return (type*) _pa_xnew_internal((n), sizeof(type));
   --  arg-macro: function pa_xnew0 (type, n)
   --    return (type*) _pa_xnew0_internal((n), sizeof(type));
   --  arg-macro: function pa_xnewdup (type, p, n)
   --    return (type*) _pa_xnewdup_internal((p), (n), sizeof(type));
   --  arg-macro: function pa_xrenew (type, p, n)
   --    return (type*) _pa_xrenew_internal(p, (n), sizeof(type));
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
  -- * Memory allocation functions.
  --  

  --* Allocate the specified number of bytes, just like malloc() does. However, in case of OOM, terminate  
   function pa_xmalloc (l : stddef_h.size_t) return System.Address  -- /usr/include/pulse/xmalloc.h:39
   with Import => True, 
        Convention => C, 
        External_Name => "pa_xmalloc";

  --* Same as pa_xmalloc(), but initialize allocated memory to 0  
   function pa_xmalloc0 (l : stddef_h.size_t) return System.Address  -- /usr/include/pulse/xmalloc.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "pa_xmalloc0";

  --*  The combination of pa_xmalloc() and realloc()  
   function pa_xrealloc (ptr : System.Address; size : stddef_h.size_t) return System.Address  -- /usr/include/pulse/xmalloc.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "pa_xrealloc";

  --* Free allocated memory  
   procedure pa_xfree (p : System.Address)  -- /usr/include/pulse/xmalloc.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "pa_xfree";

  --* Duplicate the specified string, allocating memory with pa_xmalloc()  
   function pa_xstrdup (s : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/xmalloc.h:51
   with Import => True, 
        Convention => C, 
        External_Name => "pa_xstrdup";

  --* Duplicate the specified string, but truncate after l characters  
   function pa_xstrndup (s : Interfaces.C.Strings.chars_ptr; l : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/xmalloc.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "pa_xstrndup";

  --* Duplicate the specified memory block  
   function pa_xmemdup (p : System.Address; l : stddef_h.size_t) return System.Address  -- /usr/include/pulse/xmalloc.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "pa_xmemdup";

  --* Internal helper for pa_xnew()  
   --  skipped func _pa_xnew_internal

  --* Allocate n new structures of the specified type.  
  --* Internal helper for pa_xnew0()  
   --  skipped func _pa_xnew0_internal

  --* Same as pa_xnew() but set the memory to zero  
  --* Internal helper for pa_xnew0()  
   --  skipped func _pa_xnewdup_internal

  --* Same as pa_xnew() but duplicate the specified data  
  --* Internal helper for pa_xrenew()  
   --  skipped func _pa_xrenew_internal

  --* Reallocate n new structures of the specified type.  
end pulse_xmalloc_h;
