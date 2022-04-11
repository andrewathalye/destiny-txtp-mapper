pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;
with stddef_h;

package bits_types_stack_t_h is

  -- Define stack_t.  Linux version.
  --   Copyright (C) 1998-2021 Free Software Foundation, Inc.
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

  -- Structure describing a signal stack.   
   type stack_t is record
      ss_sp : System.Address;  -- /usr/include/bits/types/stack_t.h:28
      ss_flags : aliased int;  -- /usr/include/bits/types/stack_t.h:29
      ss_size : aliased stddef_h.size_t;  -- /usr/include/bits/types/stack_t.h:30
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/types/stack_t.h:31

end bits_types_stack_t_h;