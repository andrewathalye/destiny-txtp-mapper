pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_types_h;
with Interfaces.C.Extensions;
with bits_types_stack_t_h;
with bits_types_sigset_t_h;

package sys_ucontext_h is

   --  unsupported macro: NGREG __NGREG
  -- Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

  -- Type for general register.   
   subtype greg_t is Long_Long_Integer;  -- /usr/include/sys/ucontext.h:37

  -- Number of general registers.   
  -- Container for all general registers.   
   type gregset_t is array (0 .. 22) of aliased greg_t;  -- /usr/include/sys/ucontext.h:46

  -- Number of each register in the `gregset_t' array.   
  -- Actually short cs, gs, fs, __pad0.   
   type anon1202_array1164 is array (0 .. 3) of aliased unsigned_short;
   type anon1202_array1167 is array (0 .. 2) of aliased unsigned_short;
   type u_libc_fpxreg is record
      significand : aliased anon1202_array1164;  -- /usr/include/sys/ucontext.h:103
      exponent : aliased unsigned_short;  -- /usr/include/sys/ucontext.h:104
      uu_glibc_reserved1 : aliased anon1202_array1167;  -- /usr/include/sys/ucontext.h:105
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/sys/ucontext.h:101

   type anon1203_array1169 is array (0 .. 3) of aliased bits_types_h.uu_uint32_t;
   type u_libc_xmmreg is record
      element : aliased anon1203_array1169;  -- /usr/include/sys/ucontext.h:110
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/sys/ucontext.h:108

  -- 64-bit FXSAVE format.   
   type anon1204_array1205 is array (0 .. 7) of aliased u_libc_fpxreg;
   type anon1204_array1206 is array (0 .. 15) of aliased u_libc_xmmreg;
   type anon1204_array1175 is array (0 .. 23) of aliased bits_types_h.uu_uint32_t;
   type u_libc_fpstate is record
      cwd : aliased bits_types_h.uu_uint16_t;  -- /usr/include/sys/ucontext.h:116
      swd : aliased bits_types_h.uu_uint16_t;  -- /usr/include/sys/ucontext.h:117
      ftw : aliased bits_types_h.uu_uint16_t;  -- /usr/include/sys/ucontext.h:118
      fop : aliased bits_types_h.uu_uint16_t;  -- /usr/include/sys/ucontext.h:119
      rip : aliased bits_types_h.uu_uint64_t;  -- /usr/include/sys/ucontext.h:120
      rdp : aliased bits_types_h.uu_uint64_t;  -- /usr/include/sys/ucontext.h:121
      mxcsr : aliased bits_types_h.uu_uint32_t;  -- /usr/include/sys/ucontext.h:122
      mxcr_mask : aliased bits_types_h.uu_uint32_t;  -- /usr/include/sys/ucontext.h:123
      u_st : aliased anon1204_array1205;  -- /usr/include/sys/ucontext.h:124
      u_xmm : aliased anon1204_array1206;  -- /usr/include/sys/ucontext.h:125
      uu_glibc_reserved1 : aliased anon1204_array1175;  -- /usr/include/sys/ucontext.h:126
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/sys/ucontext.h:113

  -- Structure to describe FPU registers.   
   type fpregset_t is access all u_libc_fpstate;  -- /usr/include/sys/ucontext.h:130

  -- Context to describe whole processor state.   
  -- Note that fpregs is a pointer.   
   type mcontext_t_array1210 is array (0 .. 7) of aliased Extensions.unsigned_long_long;
   type mcontext_t is record
      gregs : aliased gregset_t;  -- /usr/include/sys/ucontext.h:135
      fpregs : fpregset_t;  -- /usr/include/sys/ucontext.h:137
      uu_reserved1 : aliased mcontext_t_array1210;  -- /usr/include/sys/ucontext.h:138
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/sys/ucontext.h:139

  -- Userlevel context.   
   type ucontext_t;
   type anon1212_array1214 is array (0 .. 3) of aliased Extensions.unsigned_long_long;
   type ucontext_t is record
      uc_flags : aliased unsigned_long;  -- /usr/include/sys/ucontext.h:144
      uc_link : access ucontext_t;  -- /usr/include/sys/ucontext.h:145
      uc_stack : aliased bits_types_stack_t_h.stack_t;  -- /usr/include/sys/ucontext.h:146
      uc_mcontext : aliased mcontext_t;  -- /usr/include/sys/ucontext.h:147
      uc_sigmask : aliased bits_types_sigset_t_h.sigset_t;  -- /usr/include/sys/ucontext.h:148
      uu_fpregs_mem : aliased u_libc_fpstate;  -- /usr/include/sys/ucontext.h:149
      uu_ssp : aliased anon1212_array1214;  -- /usr/include/sys/ucontext.h:150
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/sys/ucontext.h:142

  -- Type for general register.   
  -- Number of general registers.   
  -- Container for all general registers.   
  -- Number of each register is the `gregset_t' array.   
  -- Definitions taken from the kernel headers.   
  -- Structure to describe FPU registers.   
  -- Context to describe whole processor state.   
  -- Due to Linux's history we have to use a pointer here.  The SysV/i386
  --       ABI requires a struct with the values.   

  -- Userlevel context.   
end sys_ucontext_h;
