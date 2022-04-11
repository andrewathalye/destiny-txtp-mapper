pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_stdint_uintn_h;
with stddef_h;
with Interfaces.C.Strings;

package pulse_sample_h is

   PA_CHANNELS_MAX : constant := 32;  --  /usr/include/pulse/sample.h:128

   PA_RATE_MAX : constant := (48000*8);  --  /usr/include/pulse/sample.h:131
   --  unsupported macro: PA_SAMPLE_S16NE PA_SAMPLE_S16LE
   --  unsupported macro: PA_SAMPLE_FLOAT32NE PA_SAMPLE_FLOAT32LE
   --  unsupported macro: PA_SAMPLE_S32NE PA_SAMPLE_S32LE
   --  unsupported macro: PA_SAMPLE_S24NE PA_SAMPLE_S24LE
   --  unsupported macro: PA_SAMPLE_S24_32NE PA_SAMPLE_S24_32LE
   --  unsupported macro: PA_SAMPLE_S16RE PA_SAMPLE_S16BE
   --  unsupported macro: PA_SAMPLE_FLOAT32RE PA_SAMPLE_FLOAT32BE
   --  unsupported macro: PA_SAMPLE_S32RE PA_SAMPLE_S32BE
   --  unsupported macro: PA_SAMPLE_S24RE PA_SAMPLE_S24BE
   --  unsupported macro: PA_SAMPLE_S24_32RE PA_SAMPLE_S24_32BE
   --  unsupported macro: PA_SAMPLE_FLOAT32 PA_SAMPLE_FLOAT32NE
   --  unsupported macro: PA_SAMPLE_U8 PA_SAMPLE_U8
   --  unsupported macro: PA_SAMPLE_ALAW PA_SAMPLE_ALAW
   --  unsupported macro: PA_SAMPLE_ULAW PA_SAMPLE_ULAW
   --  unsupported macro: PA_SAMPLE_S16LE PA_SAMPLE_S16LE
   --  unsupported macro: PA_SAMPLE_S16BE PA_SAMPLE_S16BE
   --  unsupported macro: PA_SAMPLE_FLOAT32LE PA_SAMPLE_FLOAT32LE
   --  unsupported macro: PA_SAMPLE_FLOAT32BE PA_SAMPLE_FLOAT32BE
   --  unsupported macro: PA_SAMPLE_S32LE PA_SAMPLE_S32LE
   --  unsupported macro: PA_SAMPLE_S32BE PA_SAMPLE_S32BE
   --  unsupported macro: PA_SAMPLE_S24LE PA_SAMPLE_S24LE
   --  unsupported macro: PA_SAMPLE_S24BE PA_SAMPLE_S24BE
   --  unsupported macro: PA_SAMPLE_S24_32LE PA_SAMPLE_S24_32LE
   --  unsupported macro: PA_SAMPLE_S24_32BE PA_SAMPLE_S24_32BE

   PA_SAMPLE_SPEC_SNPRINT_MAX : constant := 32;  --  /usr/include/pulse/sample.h:323

   PA_BYTES_SNPRINT_MAX : constant := 11;  --  /usr/include/pulse/sample.h:333
   --  arg-macro: procedure pa_sample_format_is_ne (f)
   --    pa_sample_format_is_le(f)
   --  arg-macro: procedure pa_sample_format_is_re (f)
   --    pa_sample_format_is_be(f)

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2004-2006 Lennart Poettering
  --  Copyright 2006 Pierre Ossman <ossman@cendio.se> for Cendio AB
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

  --* \page sample Sample Format Specifications
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * PulseAudio is capable of handling a multitude of sample formats, rates
  -- * and channels, transparently converting and mixing them as needed.
  -- *
  -- * \section format_sec Sample Format
  -- *
  -- * PulseAudio supports the following sample formats:
  -- *
  -- * \li PA_SAMPLE_U8 - Unsigned 8 bit integer PCM.
  -- * \li PA_SAMPLE_S16LE - Signed 16 integer bit PCM, little endian.
  -- * \li PA_SAMPLE_S16BE - Signed 16 integer bit PCM, big endian.
  -- * \li PA_SAMPLE_FLOAT32LE - 32 bit IEEE floating point PCM, little endian.
  -- * \li PA_SAMPLE_FLOAT32BE - 32 bit IEEE floating point PCM, big endian.
  -- * \li PA_SAMPLE_ALAW - 8 bit a-Law.
  -- * \li PA_SAMPLE_ULAW - 8 bit mu-Law.
  -- * \li PA_SAMPLE_S32LE - Signed 32 bit integer PCM, little endian.
  -- * \li PA_SAMPLE_S32BE - Signed 32 bit integer PCM, big endian.
  -- * \li PA_SAMPLE_S24LE - Signed 24 bit integer PCM packed, little endian.
  -- * \li PA_SAMPLE_S24BE - Signed 24 bit integer PCM packed, big endian.
  -- * \li PA_SAMPLE_S24_32LE - Signed 24 bit integer PCM in LSB of 32 bit words, little endian.
  -- * \li PA_SAMPLE_S24_32BE - Signed 24 bit integer PCM in LSB of 32 bit words, big endian.
  -- *
  -- * The floating point sample formats have the range from -1.0 to 1.0.
  -- *
  -- * The sample formats that are sensitive to endianness have convenience
  -- * macros for native endian (NE), and reverse endian (RE).
  -- *
  -- * \section rate_sec Sample Rates
  -- *
  -- * PulseAudio supports any sample rate between 1 Hz and 192000 Hz. There is no
  -- * point trying to exceed the sample rate of the output device though as the
  -- * signal will only get downsampled, consuming CPU on the machine running the
  -- * server.
  -- *
  -- * \section chan_sec Channels
  -- *
  -- * PulseAudio supports up to 32 individual channels. The order of the
  -- * channels is up to the application, but they must be continuous. To map
  -- * channels to speakers, see \ref channelmap.
  -- *
  -- * \section calc_sec Calculations
  -- *
  -- * The PulseAudio library contains a number of convenience functions to do
  -- * calculations on sample formats:
  -- *
  -- * \li pa_bytes_per_second() - The number of bytes one second of audio will
  -- *                             take given a sample format.
  -- * \li pa_frame_size() - The size, in bytes, of one frame (i.e. one set of
  -- *                       samples, one for each channel).
  -- * \li pa_sample_size() - The size, in bytes, of one sample.
  -- * \li pa_bytes_to_usec() - Calculate the time it would take to play a buffer
  -- *                          of a certain size.
  -- *
  -- * \section util_sec Convenience Functions
  -- *
  -- * The library also contains a couple of other convenience functions:
  -- *
  -- * \li pa_sample_spec_valid() - Tests if a sample format specification is
  -- *                              valid.
  -- * \li pa_sample_spec_equal() - Tests if the sample format specifications are
  -- *                              identical.
  -- * \li pa_sample_format_to_string() - Return a textual description of a
  -- *                                    sample format.
  -- * \li pa_parse_sample_format() - Parse a text string into a sample format.
  -- * \li pa_sample_spec_snprint() - Create a textual description of a complete
  -- *                                 sample format specification.
  -- * \li pa_bytes_snprint() - Pretty print a byte value (e.g. 2.5 MiB).
  --  

  --* \file
  -- * Constants and routines for sample type handling
  -- *
  -- * See also \subpage sample
  --  

  -- On Sparc, WORDS_BIGENDIAN needs to be set if _BIG_ENDIAN is defined.  
  --* Maximum number of allowed channels  
  --* Maximum allowed sample rate  
  --* Sample format  
   subtype pa_sample_format is int;
   PA_SAMPLE_U8 : constant pa_sample_format := 0;
   PA_SAMPLE_ALAW : constant pa_sample_format := 1;
   PA_SAMPLE_ULAW : constant pa_sample_format := 2;
   PA_SAMPLE_S16LE : constant pa_sample_format := 3;
   PA_SAMPLE_S16BE : constant pa_sample_format := 4;
   PA_SAMPLE_FLOAT32LE : constant pa_sample_format := 5;
   PA_SAMPLE_FLOAT32BE : constant pa_sample_format := 6;
   PA_SAMPLE_S32LE : constant pa_sample_format := 7;
   PA_SAMPLE_S32BE : constant pa_sample_format := 8;
   PA_SAMPLE_S24LE : constant pa_sample_format := 9;
   PA_SAMPLE_S24BE : constant pa_sample_format := 10;
   PA_SAMPLE_S24_32LE : constant pa_sample_format := 11;
   PA_SAMPLE_S24_32BE : constant pa_sample_format := 12;
   PA_SAMPLE_MAX : constant pa_sample_format := 13;
   PA_SAMPLE_INVALID : constant pa_sample_format := -1;  -- /usr/include/pulse/sample.h:134

  --*< Unsigned 8 Bit PCM  
  --*< 8 Bit a-Law  
  --*< 8 Bit mu-Law  
  --*< Signed 16 Bit PCM, little endian (PC)  
  --*< Signed 16 Bit PCM, big endian  
  --*< 32 Bit IEEE floating point, little endian (PC), range -1.0 to 1.0  
  --*< 32 Bit IEEE floating point, big endian, range -1.0 to 1.0  
  --*< Signed 32 Bit PCM, little endian (PC)  
  --*< Signed 32 Bit PCM, big endian  
  --*< Signed 24 Bit PCM packed, little endian (PC). \since 0.9.15  
  --*< Signed 24 Bit PCM packed, big endian. \since 0.9.15  
  --*< Signed 24 Bit PCM in LSB of 32 Bit words, little endian (PC). \since 0.9.15  
  --*< Signed 24 Bit PCM in LSB of 32 Bit words, big endian. \since 0.9.15  
  -- Remeber to update
  --     * https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/User/SupportedAudioFormats/
  --     * when adding new formats!  

  --*< Upper limit of valid sample types  
  --*< An invalid value  
   subtype pa_sample_format_t is pa_sample_format;  -- /usr/include/pulse/sample.h:183

  --* Signed 16 Bit PCM, native endian  
  --* 32 Bit IEEE floating point, native endian  
  --* Signed 32 Bit PCM, native endian  
  --* Signed 24 Bit PCM packed, native endian. \since 0.9.15  
  --* Signed 24 Bit PCM in LSB of 32 Bit words, native endian. \since 0.9.15  
  --* Signed 16 Bit PCM reverse endian  
  --* 32 Bit IEEE floating point, reverse endian  
  --* Signed 32 Bit PCM, reverse endian  
  --* Signed 24 Bit PCM, packed reverse endian. \since 0.9.15  
  --* Signed 24 Bit PCM, in LSB of 32 Bit words, reverse endian. \since 0.9.15  
  --* Signed 16 Bit PCM, native endian  
  --* 32 Bit IEEE floating point, native endian  
  --* Signed 32 Bit PCM, native endian  
  --* Signed 24 Bit PCM packed, native endian. \since 0.9.15  
  --* Signed 24 Bit PCM in LSB of 32 Bit words, native endian. \since 0.9.15  
  --* Signed 16 Bit PCM, reverse endian  
  --* 32 Bit IEEE floating point, reverse endian  
  --* Signed 32 Bit PCM, reverse endian  
  --* Signed 24 Bit PCM, packed reverse endian. \since 0.9.15  
  --* Signed 24 Bit PCM, in LSB of 32 Bit words, reverse endian. \since 0.9.15  
  --* A Shortcut for PA_SAMPLE_FLOAT32NE  
  --* \cond fulldocs  
  -- Allow clients to check with #ifdef for these sample formats  
  --* \endcond  
  --* A sample format and attribute specification  
   type pa_sample_spec is record
      format : aliased pa_sample_format_t;  -- /usr/include/pulse/sample.h:253
      rate : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/sample.h:256
      channels : aliased bits_stdint_uintn_h.uint8_t;  -- /usr/include/pulse/sample.h:259
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/sample.h:252

  --*< The sample format  
  --*< The sample rate. (e.g. 44100)  
  --*< Audio channels. (1 for mono, 2 for stereo, ...)  
  --* Type for usec specifications (unsigned). Always 64 bit.  
   subtype pa_usec_t is bits_stdint_uintn_h.uint64_t;  -- /usr/include/pulse/sample.h:264

  --* Return the amount of bytes that constitute playback of one second of
  -- * audio, with the specified sample spec.  

   function pa_bytes_per_second (spec : access constant pa_sample_spec) return stddef_h.size_t  -- /usr/include/pulse/sample.h:268
   with Import => True, 
        Convention => C, 
        External_Name => "pa_bytes_per_second";

  --* Return the size of a frame with the specific sample type  
   function pa_frame_size (spec : access constant pa_sample_spec) return stddef_h.size_t  -- /usr/include/pulse/sample.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "pa_frame_size";

  --* Return the size of a sample with the specific sample type  
   function pa_sample_size (spec : access constant pa_sample_spec) return stddef_h.size_t  -- /usr/include/pulse/sample.h:274
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_size";

  --* Similar to pa_sample_size() but take a sample format instead of a
  -- * full sample spec. \since 0.9.15  

   function pa_sample_size_of_format (f : pa_sample_format_t) return stddef_h.size_t  -- /usr/include/pulse/sample.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_size_of_format";

  --* Calculate the time it would take to play a buffer of the specified
  -- * size with the specified sample type. The return value will always
  -- * be rounded down for non-integral return values.  

   function pa_bytes_to_usec (length : bits_stdint_uintn_h.uint64_t; spec : access constant pa_sample_spec) return pa_usec_t  -- /usr/include/pulse/sample.h:283
   with Import => True, 
        Convention => C, 
        External_Name => "pa_bytes_to_usec";

  --* Calculates the size of a buffer required, for playback duration
  -- * of the time specified, with the specified sample type. The
  -- * return value will always be rounded down for non-integral
  -- * return values. \since 0.9  

   function pa_usec_to_bytes (t : pa_usec_t; spec : access constant pa_sample_spec) return stddef_h.size_t  -- /usr/include/pulse/sample.h:289
   with Import => True, 
        Convention => C, 
        External_Name => "pa_usec_to_bytes";

  --* Initialize the specified sample spec and return a pointer to
  -- * it. The sample spec will have a defined state but
  -- * pa_sample_spec_valid() will fail for it. \since 0.9.13  

   function pa_sample_spec_init (spec : access pa_sample_spec) return access pa_sample_spec  -- /usr/include/pulse/sample.h:294
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_spec_init";

  --* Return non-zero if the given integer is a valid sample format. \since 5.0  
   function pa_sample_format_valid (format : unsigned) return int  -- /usr/include/pulse/sample.h:297
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_format_valid";

  --* Return non-zero if the rate is within the supported range. \since 5.0  
   function pa_sample_rate_valid (rate : bits_stdint_uintn_h.uint32_t) return int  -- /usr/include/pulse/sample.h:300
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_rate_valid";

  --* Return non-zero if the channel count is within the supported range.
  -- * \since 5.0  

   function pa_channels_valid (channels : bits_stdint_uintn_h.uint8_t) return int  -- /usr/include/pulse/sample.h:304
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channels_valid";

  --* Return non-zero when the sample type specification is valid  
   function pa_sample_spec_valid (spec : access constant pa_sample_spec) return int  -- /usr/include/pulse/sample.h:307
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_spec_valid";

  --* Return non-zero when the two sample type specifications match  
   function pa_sample_spec_equal (a : access constant pa_sample_spec; b : access constant pa_sample_spec) return int  -- /usr/include/pulse/sample.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_spec_equal";

  --* Return a descriptive string for the specified sample format. \since 0.8  
   function pa_sample_format_to_string (f : pa_sample_format_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/sample.h:313
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_format_to_string";

  --* Parse a sample format text. Inverse of pa_sample_format_to_string()  
   function pa_parse_sample_format (format : Interfaces.C.Strings.chars_ptr) return pa_sample_format_t  -- /usr/include/pulse/sample.h:316
   with Import => True, 
        Convention => C, 
        External_Name => "pa_parse_sample_format";

  --* Maximum required string length for
  -- * pa_sample_spec_snprint(). Please note that this value can change
  -- * with any release without warning and without being considered API
  -- * or ABI breakage. You should not use this definition anywhere where
  -- * it might become part of an ABI.  

  --* Pretty print a sample type specification to a string. Returns \a s.  
   function pa_sample_spec_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      spec : access constant pa_sample_spec) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/sample.h:326
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_spec_snprint";

  --* Maximum required string length for pa_bytes_snprint(). Please note
  -- * that this value can change with any release without warning and
  -- * without being considered API or ABI breakage. You should not use
  -- * this definition anywhere where it might become part of an
  -- * ABI. \since 0.9.16  

  --* Pretty print a byte size value (i.e.\ "2.5 MiB"). Returns \a s.  
   function pa_bytes_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      v : unsigned) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/sample.h:336
   with Import => True, 
        Convention => C, 
        External_Name => "pa_bytes_snprint";

  --* Returns 1 when the specified format is little endian, 0 when
  -- * big endian. Returns -1 when endianness does not apply to the
  -- * specified format, or endianess is unknown. \since 0.9.16  

   function pa_sample_format_is_le (f : pa_sample_format_t) return int  -- /usr/include/pulse/sample.h:341
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_format_is_le";

  --* Returns 1 when the specified format is big endian, 0 when
  -- * little endian. Returns -1 when endianness does not apply to the
  -- * specified format, or endianess is unknown. \since 0.9.16  

   function pa_sample_format_is_be (f : pa_sample_format_t) return int  -- /usr/include/pulse/sample.h:346
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sample_format_is_be";

  --* Returns 1 when the specified format is native endian, 0 when
  -- * not. Returns -1 when endianness does not apply to the
  -- * specified format, or endianess is unknown. \since 0.9.16  

  --* Returns 1 when the specified format is reverse endian, 0 when
  -- * native. Returns -1 when endianness does not apply to the
  -- * specified format, or endianess is unknown. \since 0.9.16  

end pulse_sample_h;
