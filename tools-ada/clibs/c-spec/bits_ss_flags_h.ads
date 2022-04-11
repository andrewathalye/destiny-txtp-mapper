pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package bits_ss_flags_h is

   --  unsupported macro: SS_ONSTACK SS_ONSTACK
   --  unsupported macro: SS_DISABLE SS_DISABLE
  -- ss_flags values for stack_t.  Linux version.
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

  -- Possible values for `ss_flags'.   
end bits_ss_flags_h;
