pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package bits_sigevent_consts_h is

   --  unsupported macro: SIGEV_SIGNAL SIGEV_SIGNAL
   --  unsupported macro: SIGEV_NONE SIGEV_NONE
   --  unsupported macro: SIGEV_THREAD SIGEV_THREAD
   --  unsupported macro: SIGEV_THREAD_ID SIGEV_THREAD_ID
  -- sigevent constants.  Linux version.
  --   Copyright (C) 1997-2021 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, see
  --   <https://www.gnu.org/licenses/>.   

  -- `sigev_notify' values.   
  -- Notify via signal.   
  -- Other notification: meaningless.   
  -- Deliver via thread creation.   
  -- Send signal to specific thread.
  --				   This is a Linux extension.   

end bits_sigevent_consts_h;
