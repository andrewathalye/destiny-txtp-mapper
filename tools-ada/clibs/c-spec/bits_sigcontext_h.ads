pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_types_h;

package bits_sigcontext_h is

   FP_XSTATE_MAGIC1 : constant := 16#46505853#;  --  /usr/include/bits/sigcontext.h:27
   FP_XSTATE_MAGIC2 : constant := 16#46505845#;  --  /usr/include/bits/sigcontext.h:28
   --  unsupported macro: FP_XSTATE_MAGIC2_SIZE sizeof (FP_XSTATE_MAGIC2)

  -- Copyright (C) 2002-2021 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, see
  --   <https://www.gnu.org/licenses/>.   

   type anon1160_array1161 is array (0 .. 6) of aliased bits_types_h.uu_uint32_t;
   type u_fpx_sw_bytes is record
      magic1 : aliased bits_types_h.uu_uint32_t;  -- /usr/include/bits/sigcontext.h:33
      extended_size : aliased bits_types_h.uu_uint32_t;  -- /usr/include/bits/sigcontext.h:34
      xstate_bv : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:35
      xstate_size : aliased bits_types_h.uu_uint32_t;  -- /usr/include/bits/sigcontext.h:36
      uu_glibc_reserved1 : aliased anon1160_array1161;  -- /usr/include/bits/sigcontext.h:37
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/sigcontext.h:31

   type anon1163_array1164 is array (0 .. 3) of aliased unsigned_short;
   type u_fpreg is record
      significand : aliased anon1163_array1164;  -- /usr/include/bits/sigcontext.h:42
      exponent : aliased unsigned_short;  -- /usr/include/bits/sigcontext.h:43
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/sigcontext.h:40

   type anon1165_array1164 is array (0 .. 3) of aliased unsigned_short;
   type anon1165_array1167 is array (0 .. 2) of aliased unsigned_short;
   type u_fpxreg is record
      significand : aliased anon1165_array1164;  -- /usr/include/bits/sigcontext.h:48
      exponent : aliased unsigned_short;  -- /usr/include/bits/sigcontext.h:49
      uu_glibc_reserved1 : aliased anon1165_array1167;  -- /usr/include/bits/sigcontext.h:50
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/sigcontext.h:46

   type anon1168_array1169 is array (0 .. 3) of aliased bits_types_h.uu_uint32_t;
   type u_xmmreg is record
      element : aliased anon1168_array1169;  -- /usr/include/bits/sigcontext.h:55
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/sigcontext.h:53

  -- Regular FPU environment.   
  -- FXSR FPU environment.   
  -- Kernel headers before 2.1.1 define a struct sigcontext_struct, but
  --   we need sigcontext.  Some packages have come to rely on
  --   sigcontext_struct being defined on 32-bit x86, so define this for
  --   their benefit.   

  -- FPU environment matching the 64-bit FXSAVE layout.   
   type anon1171_array1172 is array (0 .. 7) of aliased u_fpxreg;
   type anon1171_array1173 is array (0 .. 15) of aliased u_xmmreg;
   type anon1171_array1175 is array (0 .. 23) of aliased bits_types_h.uu_uint32_t;
   type u_fpstate is record
      cwd : aliased bits_types_h.uu_uint16_t;  -- /usr/include/bits/sigcontext.h:126
      swd : aliased bits_types_h.uu_uint16_t;  -- /usr/include/bits/sigcontext.h:127
      ftw : aliased bits_types_h.uu_uint16_t;  -- /usr/include/bits/sigcontext.h:128
      fop : aliased bits_types_h.uu_uint16_t;  -- /usr/include/bits/sigcontext.h:129
      rip : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:130
      rdp : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:131
      mxcsr : aliased bits_types_h.uu_uint32_t;  -- /usr/include/bits/sigcontext.h:132
      mxcr_mask : aliased bits_types_h.uu_uint32_t;  -- /usr/include/bits/sigcontext.h:133
      u_st : aliased anon1171_array1172;  -- /usr/include/bits/sigcontext.h:134
      u_xmm : aliased anon1171_array1173;  -- /usr/include/bits/sigcontext.h:135
      uu_glibc_reserved1 : aliased anon1171_array1175;  -- /usr/include/bits/sigcontext.h:136
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/sigcontext.h:123

   type anon1177_union1178 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            fpstate : access u_fpstate;  -- /usr/include/bits/sigcontext.h:169
         when others =>
            uu_fpstate_word : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:170
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type anon1177_array1180 is array (0 .. 7) of aliased bits_types_h.uu_uint64_t;
   type sigcontext is record
      r8 : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:141
      r9 : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:142
      r10 : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:143
      r11 : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:144
      r12 : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:145
      r13 : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:146
      r14 : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:147
      r15 : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:148
      rdi : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:149
      rsi : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:150
      rbp : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:151
      rbx : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:152
      rdx : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:153
      rax : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:154
      rcx : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:155
      rsp : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:156
      rip : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:157
      eflags : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:158
      cs : aliased unsigned_short;  -- /usr/include/bits/sigcontext.h:159
      gs : aliased unsigned_short;  -- /usr/include/bits/sigcontext.h:160
      fs : aliased unsigned_short;  -- /usr/include/bits/sigcontext.h:161
      uu_pad0 : aliased unsigned_short;  -- /usr/include/bits/sigcontext.h:162
      err : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:163
      trapno : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:164
      oldmask : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:165
      cr2 : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:166
      anon2510 : aliased anon1177_union1178;  -- /usr/include/bits/sigcontext.h:171
      uu_reserved1 : aliased anon1177_array1180;  -- /usr/include/bits/sigcontext.h:172
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/sigcontext.h:139

   type anon1182_array1183 is array (0 .. 1) of aliased bits_types_h.uu_uint64_t;
   type anon1182_array1186 is array (0 .. 4) of aliased bits_types_h.uu_uint64_t;
   type u_xsave_hdr is record
      xstate_bv : aliased bits_types_h.uu_uint64_t;  -- /usr/include/bits/sigcontext.h:179
      uu_glibc_reserved1 : aliased anon1182_array1183;  -- /usr/include/bits/sigcontext.h:180
      uu_glibc_reserved2 : aliased anon1182_array1186;  -- /usr/include/bits/sigcontext.h:181
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/sigcontext.h:177

   type anon1188_array1190 is array (0 .. 63) of aliased bits_types_h.uu_uint32_t;
   type u_ymmh_state is record
      ymmh_space : aliased anon1188_array1190;  -- /usr/include/bits/sigcontext.h:186
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/sigcontext.h:184

   type u_xstate is record
      fpstate : aliased u_fpstate;  -- /usr/include/bits/sigcontext.h:191
      xstate_hdr : aliased u_xsave_hdr;  -- /usr/include/bits/sigcontext.h:192
      ymmh : aliased u_ymmh_state;  -- /usr/include/bits/sigcontext.h:193
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/sigcontext.h:189

end bits_sigcontext_h;
