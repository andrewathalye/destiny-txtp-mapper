pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_stdint_intn_h;
with bits_stdint_uintn_h;
with stddef_h;
with Interfaces.C.Strings;
with bits_types_FILE_h;
with System;
with sys_types_h;

package streamfile_h is

   STREAMFILE_DEFAULT_BUFFER_SIZE : constant := 16#8000#;  --  /home/andrew/src/vgmstream-r1721/src/streamfile.h:48

  --* streamfile.h - definitions for buffered file reading with STREAMFILE
  -- 

  --TODO cleanup
  --NULL, allocs
  --FILE
  --string functions in meta and so on
  --off_t
  -- MSVC fixes (though mingw uses MSVCRT but not MSC_VER, maybe use AND?)  
  -- 64-bit offset is needed for banks that hit +2.5GB (like .fsb or .ktsl2stbin).
  -- * Leave as typedef to toggle since it's theoretically slower when compiled as 32-bit.
  -- * ATM it's only used in choice places until more performance tests are done.
  -- * uint32_t could be an option but needs to test when/how neg offsets are used.
  -- *
  -- * On POSIX 32-bit off_t can become off64_t by passing -D_FILE_OFFSET_BITS=64,
  -- * but not on MSVC as it doesn't have proper POSIX support, so a custom type is needed.
  -- * fseeks/tells also need to be adjusted for 64-bit support.
  --  

  --off64_t
   subtype offv_t is bits_stdint_intn_h.int64_t;  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:38

  --typedef int64_t sizev_t; // size_t int64_t off64_t
  -- Streamfiles normally use an internal buffer to increase performance, configurable
  -- * but usually of this size. Lower increases the number of freads/system calls (slower).
  -- * However some formats need to jump around causing more buffer trashing than usual,
  -- * higher may needlessly read data that may be going to be trashed.
  -- *
  -- * Value can be adjusted freely but 8k is a good enough compromise.  

  -- struct representing a file with callbacks. Code should use STREAMFILEs and not std C functions
  -- * to do file operations, as plugins may need to provide their own callbacks.
  -- * Reads from arbitrary offsets, meaning internally may need fseek equivalents during reads.  

  -- read 'length' data at 'offset' to 'dst'  
   type u_STREAMFILE;
   type u_STREAMFILE is record
      read : access function
           (arg1 : access u_STREAMFILE;
            arg2 : access bits_stdint_uintn_h.uint8_t;
            arg3 : offv_t;
            arg4 : stddef_h.size_t) return stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:55
      get_size : access function (arg1 : access u_STREAMFILE) return stddef_h.size_t;  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:58
      get_offset : access function (arg1 : access u_STREAMFILE) return offv_t;  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:61
      get_name : access procedure
           (arg1 : access u_STREAMFILE;
            arg2 : Interfaces.C.Strings.chars_ptr;
            arg3 : stddef_h.size_t);  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:64
      open : access function
           (arg1 : access u_STREAMFILE;
            arg2 : Interfaces.C.Strings.chars_ptr;
            arg3 : stddef_h.size_t) return access u_STREAMFILE;  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:67
      close : access procedure (arg1 : access u_STREAMFILE);  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:70
      stream_index : aliased int;  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:74
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:53

  -- get max offset  
  --todo: DO NOT USE, NOT RESET PROPERLY (remove?)
  -- copy current filename to name buf  
  -- open another streamfile from filename  
  -- free current STREAMFILE  
  -- Substream selection for formats with subsongs.
  --     * Not ideal here, but it was the simplest way to pass to all init_vgmstream_x functions.  

  -- 0=default/auto (first), 1=first, N=Nth  
   subtype STREAMFILE is u_STREAMFILE;  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:76

  -- All open_ fuctions should be safe to call with wrong/null parameters.
  -- * _f versions are the same but free the passed streamfile on failure and return NULL,
  -- * to ease chaining by avoiding realloc-style temp ptr verbosity  

  -- Opens a standard STREAMFILE, opening from path.
  -- * Uses stdio (FILE) for operations, thus plugins may not want to use it.  

   function open_stdio_streamfile (filename : Interfaces.C.Strings.chars_ptr) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "open_stdio_streamfile";

  -- Opens a standard STREAMFILE from a pre-opened FILE.  
   function open_stdio_streamfile_by_file (the_file : access bits_types_FILE_h.FILE; filename : Interfaces.C.Strings.chars_ptr) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "open_stdio_streamfile_by_file";

  -- Opens a STREAMFILE that does buffered IO.
  -- * Can be used when the underlying IO may be slow (like when using custom IO).
  -- * Buffer size is optional.  

   function open_buffer_streamfile (sf : access STREAMFILE; buffer_size : stddef_h.size_t) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "open_buffer_streamfile";

   function open_buffer_streamfile_f (sf : access STREAMFILE; buffer_size : stddef_h.size_t) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:93
   with Import => True, 
        Convention => C, 
        External_Name => "open_buffer_streamfile_f";

  -- Opens a STREAMFILE that doesn't close the underlying streamfile.
  -- * Calls to open won't wrap the new SF (assumes it needs to be closed).
  -- * Can be used in metas to test custom IO without closing the external SF.  

   function open_wrap_streamfile (sf : access STREAMFILE) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:98
   with Import => True, 
        Convention => C, 
        External_Name => "open_wrap_streamfile";

   function open_wrap_streamfile_f (sf : access STREAMFILE) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:99
   with Import => True, 
        Convention => C, 
        External_Name => "open_wrap_streamfile_f";

  -- Opens a STREAMFILE that clamps reads to a section of a larger streamfile.
  -- * Can be used with subfiles inside a bigger file (to fool metas, or to simplify custom IO).  

   function open_clamp_streamfile
     (sf : access STREAMFILE;
      start : offv_t;
      size : stddef_h.size_t) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:103
   with Import => True, 
        Convention => C, 
        External_Name => "open_clamp_streamfile";

   function open_clamp_streamfile_f
     (sf : access STREAMFILE;
      start : offv_t;
      size : stddef_h.size_t) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:104
   with Import => True, 
        Convention => C, 
        External_Name => "open_clamp_streamfile_f";

  -- Opens a STREAMFILE that uses custom IO for streamfile reads.
  -- * Can be used to modify data on the fly (ex. decryption), or even transform it from a format to another. 
  -- * Data is an optional state struct of some size what will be malloc+copied on open.  

   function open_io_streamfile
     (sf : access STREAMFILE;
      data : System.Address;
      data_size : stddef_h.size_t;
      read_callback : System.Address;
      size_callback : System.Address) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:109
   with Import => True, 
        Convention => C, 
        External_Name => "open_io_streamfile";

   function open_io_streamfile_f
     (sf : access STREAMFILE;
      data : System.Address;
      data_size : stddef_h.size_t;
      read_callback : System.Address;
      size_callback : System.Address) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:110
   with Import => True, 
        Convention => C, 
        External_Name => "open_io_streamfile_f";

  -- Same, but calls init on SF open and close on close, when malloc/free is needed.
  -- * Data struct may be used to hold malloc'd pointers and stuff.  

   function open_io_streamfile_ex
     (sf : access STREAMFILE;
      data : System.Address;
      data_size : stddef_h.size_t;
      read_callback : System.Address;
      size_callback : System.Address;
      init_callback : System.Address;
      close_callback : System.Address) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:113
   with Import => True, 
        Convention => C, 
        External_Name => "open_io_streamfile_ex";

   function open_io_streamfile_ex_f
     (sf : access STREAMFILE;
      data : System.Address;
      data_size : stddef_h.size_t;
      read_callback : System.Address;
      size_callback : System.Address;
      init_callback : System.Address;
      close_callback : System.Address) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:114
   with Import => True, 
        Convention => C, 
        External_Name => "open_io_streamfile_ex_f";

  -- Opens a STREAMFILE that reports a fake name, but still re-opens itself properly.
  -- * Can be used to trick a meta's extension check (to call from another, with a modified SF).
  -- * When fakename isn't supplied it's read from the streamfile, and the extension swapped with fakeext.
  -- * If the fakename is an existing file, open won't work on it as it'll reopen the fake-named streamfile.  

   function open_fakename_streamfile
     (sf : access STREAMFILE;
      fakename : Interfaces.C.Strings.chars_ptr;
      fakeext : Interfaces.C.Strings.chars_ptr) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "open_fakename_streamfile";

   function open_fakename_streamfile_f
     (sf : access STREAMFILE;
      fakename : Interfaces.C.Strings.chars_ptr;
      fakeext : Interfaces.C.Strings.chars_ptr) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:121
   with Import => True, 
        Convention => C, 
        External_Name => "open_fakename_streamfile_f";

  -- Opens streamfile formed from multiple streamfiles, their data joined during reads.
  -- * Can be used when data is segmented in multiple separate files.
  -- * The first streamfile is used to get names, stream index and so on.  

   function open_multifile_streamfile (sfs : System.Address; sfs_size : stddef_h.size_t) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:126
   with Import => True, 
        Convention => C, 
        External_Name => "open_multifile_streamfile";

   function open_multifile_streamfile_f (sfs : System.Address; sfs_size : stddef_h.size_t) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:127
   with Import => True, 
        Convention => C, 
        External_Name => "open_multifile_streamfile_f";

  -- Opens a STREAMFILE from a (path)+filename.
  -- * Just a wrapper, to avoid having to access the STREAMFILE's callbacks directly.  

   function open_streamfile (sf : access STREAMFILE; pathname : Interfaces.C.Strings.chars_ptr) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "open_streamfile";

  -- Opens a STREAMFILE from a base pathname + new extension
  -- * Can be used to get companion headers.  

   function open_streamfile_by_ext (sf : access STREAMFILE; ext : Interfaces.C.Strings.chars_ptr) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:135
   with Import => True, 
        Convention => C, 
        External_Name => "open_streamfile_by_ext";

  -- Opens a STREAMFILE from a base path + new filename.
  -- * Can be used to get companion files. Relative paths like
  -- * './filename', '../filename', 'dir/filename' also work.  

   function open_streamfile_by_filename (sf : access STREAMFILE; filename : Interfaces.C.Strings.chars_ptr) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:140
   with Import => True, 
        Convention => C, 
        External_Name => "open_streamfile_by_filename";

  -- Reopen a STREAMFILE with a different buffer size, for fine-tuned bigfile parsing.
  -- * Uses default buffer size when buffer_size is 0  

   function reopen_streamfile (sf : access STREAMFILE; buffer_size : stddef_h.size_t) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "reopen_streamfile";

  -- close a file, destroy the STREAMFILE object  
   procedure close_streamfile (sf : access STREAMFILE)  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:148
   with Import => True, 
        Convention => C, 
        External_Name => "close_streamfile";

  -- read from a file, returns number of bytes read  
   function read_streamfile
     (dst : access bits_stdint_uintn_h.uint8_t;
      offset : offv_t;
      length : stddef_h.size_t;
      sf : access STREAMFILE) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:154
   with Import => True, 
        Convention => C, 
        External_Name => "read_streamfile";

  -- return file size  
   function get_streamfile_size (sf : access STREAMFILE) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:159
   with Import => True, 
        Convention => C, 
        External_Name => "get_streamfile_size";

  -- Sometimes you just need an int, and we're doing the buffering.
  --* Note, however, that if these fail to read they'll return -1,
  --* so that should not be a valid value or there should be some backup.  

   function read_16bitLE (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int16_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:167
   with Import => True, 
        Convention => C, 
        External_Name => "read_16bitLE";

   function read_16bitBE (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int16_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:173
   with Import => True, 
        Convention => C, 
        External_Name => "read_16bitBE";

   function read_32bitLE (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:179
   with Import => True, 
        Convention => C, 
        External_Name => "read_32bitLE";

   function read_32bitBE (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:185
   with Import => True, 
        Convention => C, 
        External_Name => "read_32bitBE";

   function read_64bitLE (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int64_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:191
   with Import => True, 
        Convention => C, 
        External_Name => "read_64bitLE";

   function read_64bitBE (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int64_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:197
   with Import => True, 
        Convention => C, 
        External_Name => "read_64bitBE";

   function read_8bit (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int8_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:203
   with Import => True, 
        Convention => C, 
        External_Name => "read_8bit";

  -- alias of the above  
   function read_s8 (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int8_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "read_s8";

   function read_u8 (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_uintn_h.uint8_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:212
   with Import => True, 
        Convention => C, 
        External_Name => "read_u8";

   function read_s16le (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int16_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:213
   with Import => True, 
        Convention => C, 
        External_Name => "read_s16le";

   function read_u16le (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_uintn_h.uint16_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:214
   with Import => True, 
        Convention => C, 
        External_Name => "read_u16le";

   function read_s16be (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int16_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:215
   with Import => True, 
        Convention => C, 
        External_Name => "read_s16be";

   function read_u16be (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_uintn_h.uint16_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:216
   with Import => True, 
        Convention => C, 
        External_Name => "read_u16be";

   function read_s32le (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:217
   with Import => True, 
        Convention => C, 
        External_Name => "read_s32le";

   function read_u32le (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_uintn_h.uint32_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:218
   with Import => True, 
        Convention => C, 
        External_Name => "read_u32le";

   function read_s32be (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int32_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:219
   with Import => True, 
        Convention => C, 
        External_Name => "read_s32be";

   function read_u32be (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_uintn_h.uint32_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:220
   with Import => True, 
        Convention => C, 
        External_Name => "read_u32be";

   function read_s64be (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int64_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:221
   with Import => True, 
        Convention => C, 
        External_Name => "read_s64be";

   function read_u64be (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_uintn_h.uint64_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:222
   with Import => True, 
        Convention => C, 
        External_Name => "read_u64be";

   function read_s64le (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_intn_h.int64_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:223
   with Import => True, 
        Convention => C, 
        External_Name => "read_s64le";

   function read_u64le (offset : sys_types_h.off_t; sf : access STREAMFILE) return bits_stdint_uintn_h.uint64_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "read_u64le";

   function read_f32be (offset : sys_types_h.off_t; sf : access STREAMFILE) return float  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:226
   with Import => True, 
        Convention => C, 
        External_Name => "read_f32be";

   function read_f32le (offset : sys_types_h.off_t; sf : access STREAMFILE) return float  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:233
   with Import => True, 
        Convention => C, 
        External_Name => "read_f32le";

  -- on GCC, this reader will be correctly optimized out (as long as it's static/inline), would be same as declaring:
  -- uintXX_t (*read_uXX)(off_t,uint8_t*) = be ? get_uXXbe : get_uXXle;
  -- only for the functions actually used in code, and inlined if possible (like big_endian param being a constant).
  -- on MSVC seems all read_X in sf_reader are compiled and included in the translation unit, plus ignores constants
  -- so may result on bloatness?
  -- (from godbolt tests, test more real cases)
  -- collection of callbacks for quick access  
  --maybe r.s32
  -- ...  
  -- sf_reader r;
  -- * ...
  -- * sf_reader_init(&r, big_endian);
  -- * val = r.read_s32; //maybe r.s32?
  --  

  --align32, align16, clamp16, etc
  -- fastest to compare would be read_u32x == (uint32), but should be pre-optimized (see get_id32x)  
  --const 
   function is_id32be
     (offset : sys_types_h.off_t;
      sf : access STREAMFILE;
      s : Interfaces.C.Strings.chars_ptr) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:295
   with Import => True, 
        Convention => C, 
        External_Name => "is_id32be";

  --const 
   function is_id32le
     (offset : sys_types_h.off_t;
      sf : access STREAMFILE;
      s : Interfaces.C.Strings.chars_ptr) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:299
   with Import => True, 
        Convention => C, 
        External_Name => "is_id32le";

  --const 
   function is_id64be
     (offset : sys_types_h.off_t;
      sf : access STREAMFILE;
      s : Interfaces.C.Strings.chars_ptr) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:303
   with Import => True, 
        Convention => C, 
        External_Name => "is_id64be";

  --TODO: maybe move to streamfile.c
  -- guess byte endianness from a given value, return true if big endian and false if little endian  
   function guess_endianness16bit (offset : sys_types_h.off_t; sf : access STREAMFILE) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "guess_endianness16bit";

  -- ?  
   function guess_endianness32bit (offset : sys_types_h.off_t; sf : access STREAMFILE) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:315
   with Import => True, 
        Convention => C, 
        External_Name => "guess_endianness32bit";

  -- ?  
   function align_size_to_block (value : stddef_h.size_t; block_align : stddef_h.size_t) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:321
   with Import => True, 
        Convention => C, 
        External_Name => "align_size_to_block";

  -- various STREAMFILE helpers functions  
  -- Read into dst a line delimited by CRLF (Windows) / LF (Unux) / CR (Mac) / EOF, null-terminated
  -- * and without line feeds. Returns bytes read (including CR/LF), *not* the same as string length.
  -- * p_line_ok is set to 1 if the complete line was read; pass NULL to ignore.  

   function read_line
     (buf : Interfaces.C.Strings.chars_ptr;
      buf_size : int;
      offset : sys_types_h.off_t;
      sf : access STREAMFILE;
      p_line_ok : access int) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:332
   with Import => True, 
        Convention => C, 
        External_Name => "read_line";

  -- skip BOM if needed  
   function read_bom (sf : access STREAMFILE) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:335
   with Import => True, 
        Convention => C, 
        External_Name => "read_bom";

  -- reads a c-string (ANSI only), up to bufsize or NULL, returning size. buf is optional (works as get_string_size).  
   function read_string
     (buf : Interfaces.C.Strings.chars_ptr;
      buf_size : stddef_h.size_t;
      offset : sys_types_h.off_t;
      sf : access STREAMFILE) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:338
   with Import => True, 
        Convention => C, 
        External_Name => "read_string";

  -- reads a UTF16 string... but actually only as ANSI (discards the upper byte)  
   function read_string_utf16
     (buf : Interfaces.C.Strings.chars_ptr;
      buf_size : stddef_h.size_t;
      offset : sys_types_h.off_t;
      sf : access STREAMFILE;
      big_endian : int) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:340
   with Import => True, 
        Convention => C, 
        External_Name => "read_string_utf16";

   function read_string_utf16le
     (buf : Interfaces.C.Strings.chars_ptr;
      buf_size : stddef_h.size_t;
      offset : sys_types_h.off_t;
      sf : access STREAMFILE) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:341
   with Import => True, 
        Convention => C, 
        External_Name => "read_string_utf16le";

   function read_string_utf16be
     (buf : Interfaces.C.Strings.chars_ptr;
      buf_size : stddef_h.size_t;
      offset : sys_types_h.off_t;
      sf : access STREAMFILE) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:342
   with Import => True, 
        Convention => C, 
        External_Name => "read_string_utf16be";

  -- Opens a file containing decryption keys and copies to buffer.
  -- * Tries "(name.ext)key" (per song), "(.ext)key" (per folder) keynames.
  -- * returns size of key if found and copied  

   function read_key_file
     (buf : access bits_stdint_uintn_h.uint8_t;
      buf_size : stddef_h.size_t;
      sf : access STREAMFILE) return stddef_h.size_t  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:347
   with Import => True, 
        Convention => C, 
        External_Name => "read_key_file";

  -- Opens .txtm file containing file:companion file(-s) mappings and tries to see if there's a match
  -- * then loads the associated companion file if one is found  

   function read_filemap_file (sf : access STREAMFILE; file_num : int) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:351
   with Import => True, 
        Convention => C, 
        External_Name => "read_filemap_file";

   function read_filemap_file_pos
     (sf : access STREAMFILE;
      file_num : int;
      p_pos : access int) return access STREAMFILE  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:352
   with Import => True, 
        Convention => C, 
        External_Name => "read_filemap_file_pos";

  -- hack to allow relative paths in various OSs  
   procedure fix_dir_separators (filename : Interfaces.C.Strings.chars_ptr)  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:356
   with Import => True, 
        Convention => C, 
        External_Name => "fix_dir_separators";

  -- Checks if the stream filename is one of the extensions (comma-separated, ex. "adx" or "adx,aix").
  -- * Empty is ok to accept files without extension ("", "adx,,aix"). Returns 0 on failure  

   function check_extensions (sf : access STREAMFILE; cmp_exts : Interfaces.C.Strings.chars_ptr) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:360
   with Import => True, 
        Convention => C, 
        External_Name => "check_extensions";

  -- chunk-style file helpers  
   function find_chunk_be
     (sf : access STREAMFILE;
      chunk_id : bits_stdint_uintn_h.uint32_t;
      start_offset : sys_types_h.off_t;
      full_chunk_size : int;
      p_chunk_offset : access sys_types_h.off_t;
      p_chunk_size : access stddef_h.size_t) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:363
   with Import => True, 
        Convention => C, 
        External_Name => "find_chunk_be";

   function find_chunk_le
     (sf : access STREAMFILE;
      chunk_id : bits_stdint_uintn_h.uint32_t;
      start_offset : sys_types_h.off_t;
      full_chunk_size : int;
      p_chunk_offset : access sys_types_h.off_t;
      p_chunk_size : access stddef_h.size_t) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:364
   with Import => True, 
        Convention => C, 
        External_Name => "find_chunk_le";

   function find_chunk
     (sf : access STREAMFILE;
      chunk_id : bits_stdint_uintn_h.uint32_t;
      start_offset : sys_types_h.off_t;
      full_chunk_size : int;
      p_chunk_offset : access sys_types_h.off_t;
      p_chunk_size : access stddef_h.size_t;
      big_endian_size : int;
      zero_size_end : int) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:365
   with Import => True, 
        Convention => C, 
        External_Name => "find_chunk";

  -- find a RIFF-style chunk (with chunk_size not including id and size)  
   function find_chunk_riff_le
     (sf : access STREAMFILE;
      chunk_id : bits_stdint_uintn_h.uint32_t;
      start_offset : sys_types_h.off_t;
      max_size : stddef_h.size_t;
      p_chunk_offset : access sys_types_h.off_t;
      p_chunk_size : access stddef_h.size_t) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:367
   with Import => True, 
        Convention => C, 
        External_Name => "find_chunk_riff_le";

   function find_chunk_riff_be
     (sf : access STREAMFILE;
      chunk_id : bits_stdint_uintn_h.uint32_t;
      start_offset : sys_types_h.off_t;
      max_size : stddef_h.size_t;
      p_chunk_offset : access sys_types_h.off_t;
      p_chunk_size : access stddef_h.size_t) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:368
   with Import => True, 
        Convention => C, 
        External_Name => "find_chunk_riff_be";

  -- same with chunk ids in variable endianess (so instead of "fmt " has " tmf"  
   function find_chunk_riff_ve
     (sf : access STREAMFILE;
      chunk_id : bits_stdint_uintn_h.uint32_t;
      start_offset : sys_types_h.off_t;
      max_size : stddef_h.size_t;
      p_chunk_offset : access sys_types_h.off_t;
      p_chunk_size : access stddef_h.size_t;
      big_endian : int) return int  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:370
   with Import => True, 
        Convention => C, 
        External_Name => "find_chunk_riff_ve";

  -- filename helpers  
   procedure get_streamfile_name
     (sf : access STREAMFILE;
      buf : Interfaces.C.Strings.chars_ptr;
      size : stddef_h.size_t)  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:373
   with Import => True, 
        Convention => C, 
        External_Name => "get_streamfile_name";

   procedure get_streamfile_filename
     (sf : access STREAMFILE;
      buf : Interfaces.C.Strings.chars_ptr;
      size : stddef_h.size_t)  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:374
   with Import => True, 
        Convention => C, 
        External_Name => "get_streamfile_filename";

   procedure get_streamfile_basename
     (sf : access STREAMFILE;
      buf : Interfaces.C.Strings.chars_ptr;
      size : stddef_h.size_t)  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:375
   with Import => True, 
        Convention => C, 
        External_Name => "get_streamfile_basename";

   procedure get_streamfile_path
     (sf : access STREAMFILE;
      buf : Interfaces.C.Strings.chars_ptr;
      size : stddef_h.size_t)  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:376
   with Import => True, 
        Convention => C, 
        External_Name => "get_streamfile_path";

   procedure get_streamfile_ext
     (sf : access STREAMFILE;
      buf : Interfaces.C.Strings.chars_ptr;
      size : stddef_h.size_t)  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:377
   with Import => True, 
        Convention => C, 
        External_Name => "get_streamfile_ext";

   procedure dump_streamfile (sf : access STREAMFILE; num : int)  -- /home/andrew/src/vgmstream-r1721/src/streamfile.h:379
   with Import => True, 
        Convention => C, 
        External_Name => "dump_streamfile";

end streamfile_h;
