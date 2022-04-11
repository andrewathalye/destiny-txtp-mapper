pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;

package bits_types_u_sigval_t_h is

  -- Define __sigval_t.
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

  -- Type for data associated with a signal.   
   type sigval (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            sival_int : aliased int;  -- /usr/include/bits/types/__sigval_t.h:26
         when others =>
            sival_ptr : System.Address;  -- /usr/include/bits/types/__sigval_t.h:27
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;  -- /usr/include/bits/types/__sigval_t.h:24

   subtype uu_sigval_t is sigval;  -- /usr/include/bits/types/__sigval_t.h:30

end bits_types_u_sigval_t_h;
