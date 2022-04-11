pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;

with bits_stdint_uintn_h;
with bits_stdint_intn_h;
with Interfaces.C.Strings;
with streamtypes_h;

with VGMStream_Common; use VGMStream_Common;

package util_h is

   --  unsupported macro: put_u8 put_8bit
   --  unsupported macro: put_u16le put_16bitLE
   --  unsupported macro: put_u32le put_32bitLE
   --  unsupported macro: put_u16be put_16bitBE
   --  unsupported macro: put_u32be put_32bitBE
   --  unsupported macro: put_s8 put_8bit
   --  unsupported macro: put_s16le put_16bitLE
   --  unsupported macro: put_s32le put_32bitLE
   --  unsupported macro: put_s16be put_16bitBE
   --  unsupported macro: put_s32be put_32bitBE
  -- * util.h - utility functions
  --  

  -- very common functions, so static (inline) in .h as compiler can optimize to avoid some call overhead  
  -- host endian independent multi-byte integer reading  
   function get_16bitBE (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int16_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:14
   with Import => True, 
        Convention => C, 
        External_Name => "get_16bitBE";

   function get_16bitLE (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int16_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:18
   with Import => True, 
        Convention => C, 
        External_Name => "get_16bitLE";

   function get_32bitBE (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:22
   with Import => True, 
        Convention => C, 
        External_Name => "get_32bitBE";

   function get_32bitLE (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:26
   with Import => True, 
        Convention => C, 
        External_Name => "get_32bitLE";

   function get_64bitBE (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int64_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:30
   with Import => True, 
        Convention => C, 
        External_Name => "get_64bitBE";

   function get_64bitLE (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int64_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:34
   with Import => True, 
        Convention => C, 
        External_Name => "get_64bitLE";

  -- alias of the above  
   function get_s8 (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int8_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:39
   with Import => True, 
        Convention => C, 
        External_Name => "get_s8";

   function get_u8 (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_uintn_h.uint8_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:40
   with Import => True, 
        Convention => C, 
        External_Name => "get_u8";

   function get_s16le (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int16_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:41
   with Import => True, 
        Convention => C, 
        External_Name => "get_s16le";

   function get_u16le (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_uintn_h.uint16_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "get_u16le";

   function get_s16be (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int16_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:43
   with Import => True, 
        Convention => C, 
        External_Name => "get_s16be";

   function get_u16be (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_uintn_h.uint16_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "get_u16be";

   function get_s32le (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "get_s32le";

   function get_u32le (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_uintn_h.uint32_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:46
   with Import => True, 
        Convention => C, 
        External_Name => "get_u32le";

   function get_s32be (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "get_s32be";

   function get_u32be (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_uintn_h.uint32_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "get_u32be";

   function get_s64le (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int64_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "get_s64le";

   function get_u64le (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_uintn_h.uint64_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "get_u64le";

   function get_s64be (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_intn_h.int64_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:51
   with Import => True, 
        Convention => C, 
        External_Name => "get_s64be";

   function get_u64be (p : access bits_stdint_uintn_h.uint8_t) return bits_stdint_uintn_h.uint64_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "get_u64be";

  -- The recommended int-to-float type punning in C is through union, but pointer casting
  -- * works too (though less portable due to aliasing rules?). For C++ memcpy seems
  -- * recommended. Both work in GCC and VS2015+ (not sure about older, ifdef as needed).  

   function get_f32be (p : access bits_stdint_uintn_h.uint8_t) return float  -- /home/andrew/src/vgmstream-r1721/src/util.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "get_f32be";

   function get_f32le (p : access bits_stdint_uintn_h.uint8_t) return float  -- /home/andrew/src/vgmstream-r1721/src/util.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "get_f32le";

   function get_d64be (p : access bits_stdint_uintn_h.uint8_t) return double  -- /home/andrew/src/vgmstream-r1721/src/util.h:73
   with Import => True, 
        Convention => C, 
        External_Name => "get_d64be";

   function get_d64le (p : access bits_stdint_uintn_h.uint8_t) return double  -- /home/andrew/src/vgmstream-r1721/src/util.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "get_d64le";

   procedure put_8bit (buf : access bits_stdint_uintn_h.uint8_t; i : bits_stdint_intn_h.int8_t)  -- /home/andrew/src/vgmstream-r1721/src/util.h:104
   with Import => True, 
        Convention => C, 
        External_Name => "put_8bit";

   procedure put_16bitLE (buf : access bits_stdint_uintn_h.uint8_t; i : bits_stdint_intn_h.int16_t)  -- /home/andrew/src/vgmstream-r1721/src/util.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "put_16bitLE";

   procedure put_32bitLE (buf : access bits_stdint_uintn_h.uint8_t; i : bits_stdint_intn_h.int32_t)  -- /home/andrew/src/vgmstream-r1721/src/util.h:106
   with Import => True, 
        Convention => C, 
        External_Name => "put_32bitLE";

   procedure put_16bitBE (buf : access bits_stdint_uintn_h.uint8_t; i : bits_stdint_intn_h.int16_t)  -- /home/andrew/src/vgmstream-r1721/src/util.h:107
   with Import => True, 
        Convention => C, 
        External_Name => "put_16bitBE";

   procedure put_32bitBE (buf : access bits_stdint_uintn_h.uint8_t; i : bits_stdint_intn_h.int32_t)  -- /home/andrew/src/vgmstream-r1721/src/util.h:108
   with Import => True, 
        Convention => C, 
        External_Name => "put_32bitBE";

  -- alias of the above  
  --TODO: improve
  -- signed nibbles come up a lot  
   nibble_to_int : aliased array (0 .. 15) of aliased int  -- /home/andrew/src/vgmstream-r1721/src/util.h:124
   with Import => True, 
        Convention => C, 
        External_Name => "nibble_to_int";

   function get_nibble_signed (n : bits_stdint_uintn_h.uint8_t; upper : int) return int  -- /home/andrew/src/vgmstream-r1721/src/util.h:126
   with Import => True, 
        Convention => C, 
        External_Name => "get_nibble_signed";

  --return ((n&0x70)-(n&0x80))>>4; 
   function get_high_nibble_signed (n : bits_stdint_uintn_h.uint8_t) return int  -- /home/andrew/src/vgmstream-r1721/src/util.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "get_high_nibble_signed";

  --return ((n&0x70)-(n&0x80))>>4; 
   function get_low_nibble_signed (n : bits_stdint_uintn_h.uint8_t) return int  -- /home/andrew/src/vgmstream-r1721/src/util.h:136
   with Import => True, 
        Convention => C, 
        External_Name => "get_low_nibble_signed";

  --return (n&7)-(n&8); 
   function clamp16 (val : bits_stdint_intn_h.int32_t) return int  -- /home/andrew/src/vgmstream-r1721/src/util.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "clamp16";

  -- transforms a string to uint32 (for comparison), but if this is static + all goes well
  -- * compiler should pre-calculate and use uint32 directly  

  --const 
   function get_id32be (s : Interfaces.C.Strings.chars_ptr) return bits_stdint_uintn_h.uint32_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:150
   with Import => True, 
        Convention => C, 
        External_Name => "get_id32be";

  --static inline /*const*/ uint32_t get_id32le(const char* s) {
  --    return (uint32_t)(s[0] << 0) | (s[1] << 8) | (s[2] << 16) | (s[3] << 24);
  --}
  --const 
   function get_id64be (s : Interfaces.C.Strings.chars_ptr) return bits_stdint_uintn_h.uint64_t  -- /home/andrew/src/vgmstream-r1721/src/util.h:158
   with Import => True, 
        Convention => C, 
        External_Name => "get_id64be";

  -- less common functions, no need to inline  
   function round10 (val : int) return int  -- /home/andrew/src/vgmstream-r1721/src/util.h:174
   with Import => True, 
        Convention => C, 
        External_Name => "round10";

  -- return a file's extension (a pointer to the first character of the
  -- * extension in the original filename or the ending null byte if no extension  

   function filename_extension (pathname : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /home/andrew/src/vgmstream-r1721/src/util.h:178
   with Import => True, 
        Convention => C, 
        External_Name => "filename_extension";

  -- change pathname's extension to another (or add it if extensionless)  
  --size_t 
   procedure swap_extension
     (pathname : Interfaces.C.Strings.chars_ptr;
      pathname_len : int;
      swap : Interfaces.C.Strings.chars_ptr)  -- /home/andrew/src/vgmstream-r1721/src/util.h:181
   with Import => True, 
        Convention => C, 
        External_Name => "swap_extension";

  -- swap samples in machine endianness to little endian (useful to write .wav)  
   pragma Warnings (Off, "-gnatwx"); -- C-style pointer issues
   procedure swap_samples_le (buf : Sample_Buffer_Access; count : int)  -- /home/andrew/src/vgmstream-r1721/src/util.h:184
   with Import => True, 
        Convention => C, 
        External_Name => "swap_samples_le";
   pragma Warnings (On);

   procedure concatn
     (length : int;
      dst : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr)  -- /home/andrew/src/vgmstream-r1721/src/util.h:186
   with Import => True, 
        Convention => C, 
        External_Name => "concatn";

end util_h;
