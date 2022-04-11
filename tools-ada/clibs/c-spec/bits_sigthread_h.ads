pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
limited with bits_types_u_sigset_t_h;
with bits_pthreadtypes_h;

package bits_sigthread_h is

  -- Signal handling function for threaded programs.
  --   Copyright (C) 1998-2021 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public License as
  --   published by the Free Software Foundation; either version 2.1 of the
  --   License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; see the file COPYING.LIB.  If
  --   not, see <https://www.gnu.org/licenses/>.   

  -- Functions for handling signals.  
  -- Modify the signal mask for the calling thread.  The arguments have
  --   the same meaning as for sigprocmask(2).  

   function pthread_sigmask
     (uu_how : int;
      uu_newmask : access constant bits_types_u_sigset_t_h.uu_sigset_t;
      uu_oldmask : access bits_types_u_sigset_t_h.uu_sigset_t) return int  -- /usr/include/bits/sigthread.h:31
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_sigmask";

  -- Send signal SIGNO to the given thread.  
   function pthread_kill (uu_threadid : bits_pthreadtypes_h.pthread_t; uu_signo : int) return int  -- /usr/include/bits/sigthread.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_kill";

  -- Queue signal and data to a thread.   
end bits_sigthread_h;
