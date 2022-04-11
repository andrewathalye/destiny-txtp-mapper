pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with pulse_proplist_h;
with stddef_h;
with pulse_sample_h;
limited with pulse_channelmap_h;
with System;
with bits_stdint_uintn_h;

package pulse_format_h is

   --  unsupported macro: PA_ENCODING_ANY PA_ENCODING_ANY
   --  unsupported macro: PA_ENCODING_PCM PA_ENCODING_PCM
   --  unsupported macro: PA_ENCODING_AC3_IEC61937 PA_ENCODING_AC3_IEC61937
   --  unsupported macro: PA_ENCODING_EAC3_IEC61937 PA_ENCODING_EAC3_IEC61937
   --  unsupported macro: PA_ENCODING_MPEG_IEC61937 PA_ENCODING_MPEG_IEC61937
   --  unsupported macro: PA_ENCODING_DTS_IEC61937 PA_ENCODING_DTS_IEC61937
   --  unsupported macro: PA_ENCODING_MPEG2_AAC_IEC61937 PA_ENCODING_MPEG2_AAC_IEC61937
   --  unsupported macro: PA_ENCODING_TRUEHD_IEC61937 PA_ENCODING_TRUEHD_IEC61937
   --  unsupported macro: PA_ENCODING_DTSHD_IEC61937 PA_ENCODING_DTSHD_IEC61937
   --  unsupported macro: PA_ENCODING_MAX PA_ENCODING_MAX
   --  unsupported macro: PA_ENCODING_INVALID PA_ENCODING_INVALID
   PA_FORMAT_INFO_SNPRINT_MAX : constant := 256;  --  /usr/include/pulse/format.h:137
   --  unsupported macro: PA_PROP_TYPE_INT PA_PROP_TYPE_INT
   --  unsupported macro: PA_PROP_TYPE_INT_RANGE PA_PROP_TYPE_INT_RANGE
   --  unsupported macro: PA_PROP_TYPE_INT_ARRAY PA_PROP_TYPE_INT_ARRAY
   --  unsupported macro: PA_PROP_TYPE_STRING PA_PROP_TYPE_STRING
   --  unsupported macro: PA_PROP_TYPE_STRING_ARRAY PA_PROP_TYPE_STRING_ARRAY
   --  unsupported macro: PA_PROP_TYPE_INVALID PA_PROP_TYPE_INVALID

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2011 Intel Corporation
  --  Copyright 2011 Collabora Multimedia
  --  Copyright 2011 Arun Raghavan <arun.raghavan@collabora.co.uk>
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

  --* \file
  -- * Utility functions for handling a stream or sink format.  

  --* Represents the type of encoding used in a stream or accepted by a sink. \since 1.0  
   subtype pa_encoding is int;
   PA_ENCODING_ANY : constant pa_encoding := 0;
   PA_ENCODING_PCM : constant pa_encoding := 1;
   PA_ENCODING_AC3_IEC61937 : constant pa_encoding := 2;
   PA_ENCODING_EAC3_IEC61937 : constant pa_encoding := 3;
   PA_ENCODING_MPEG_IEC61937 : constant pa_encoding := 4;
   PA_ENCODING_DTS_IEC61937 : constant pa_encoding := 5;
   PA_ENCODING_MPEG2_AAC_IEC61937 : constant pa_encoding := 6;
   PA_ENCODING_TRUEHD_IEC61937 : constant pa_encoding := 7;
   PA_ENCODING_DTSHD_IEC61937 : constant pa_encoding := 8;
   PA_ENCODING_MAX : constant pa_encoding := 9;
   PA_ENCODING_INVALID : constant pa_encoding := -1;  -- /usr/include/pulse/format.h:37

  --*< Any encoding format, PCM or compressed  
  --*< Any PCM format  
  --*< AC3 data encapsulated in IEC 61937 header/padding  
  --*< EAC3 data encapsulated in IEC 61937 header/padding  
  --*< MPEG-1 or MPEG-2 (Part 3, not AAC) data encapsulated in IEC 61937 header/padding  
  --*< DTS data encapsulated in IEC 61937 header/padding  
  --*< MPEG-2 AAC data encapsulated in IEC 61937 header/padding. \since 4.0  
  --*< Dolby TrueHD data encapsulated in IEC 61937 header/padding. \since 13.0  
  --*< DTS-HD Master Audio encapsulated in IEC 61937 header/padding. \since 13.0  
  -- Remeber to update
  --     * https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/User/SupportedAudioFormats/
  --     * when adding new encodings!  

  --*< Valid encoding types must be less than this value  
  --*< Represents an invalid encoding  
   subtype pa_encoding_t is pa_encoding;  -- /usr/include/pulse/format.h:74

  --* \cond fulldocs  
  --* \endcond  
  --* Returns a printable string representing the given encoding type. \since 1.0  
   function pa_encoding_to_string (e : pa_encoding_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/format.h:91
   with Import => True, 
        Convention => C, 
        External_Name => "pa_encoding_to_string";

  --* Converts a string of the form returned by \a pa_encoding_to_string() back to
  -- * a \a pa_encoding_t. \since 1.0  

   function pa_encoding_from_string (encoding : Interfaces.C.Strings.chars_ptr) return pa_encoding_t  -- /usr/include/pulse/format.h:95
   with Import => True, 
        Convention => C, 
        External_Name => "pa_encoding_from_string";

  --* Represents the format of data provided in a stream or processed by a sink. \since 1.0  
   type pa_format_info is record
      encoding : aliased pa_encoding_t;  -- /usr/include/pulse/format.h:99
      plist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/format.h:102
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/format.h:98

  --*< The encoding used for the format  
  --*< Additional encoding-specific properties such as sample rate, bitrate, etc.  
  --* Allocates a new \a pa_format_info structure. Clients must initialise at
  -- * least the encoding field themselves. Free with pa_format_info_free. \since 1.0  

   function pa_format_info_new return access pa_format_info  -- /usr/include/pulse/format.h:108
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_new";

  --* Returns a new \a pa_format_info struct and representing the same format as \a src. \since 1.0  
   function pa_format_info_copy (src : access constant pa_format_info) return access pa_format_info  -- /usr/include/pulse/format.h:111
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_copy";

  --* Frees a \a pa_format_info structure. \since 1.0  
   procedure pa_format_info_free (f : access pa_format_info)  -- /usr/include/pulse/format.h:114
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_free";

  --* Returns non-zero when the format info structure is valid. \since 1.0  
   function pa_format_info_valid (f : access constant pa_format_info) return int  -- /usr/include/pulse/format.h:117
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_valid";

  --* Returns non-zero when the format info structure represents a PCM
  -- * (i.e.\ uncompressed data) format. \since 1.0  

   function pa_format_info_is_pcm (f : access constant pa_format_info) return int  -- /usr/include/pulse/format.h:121
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_is_pcm";

  --* Returns non-zero if the format represented by \a first is a subset of
  -- * the format represented by \a second. This means that \a second must
  -- * have all the fields that \a first does, but the reverse need not
  -- * be true. This is typically expected to be used to check if a
  -- * stream's format is compatible with a given sink. In such a case,
  -- * \a first would be the sink's format and \a second would be the
  -- * stream's. \since 1.0  

   function pa_format_info_is_compatible (first : access constant pa_format_info; second : access constant pa_format_info) return int  -- /usr/include/pulse/format.h:130
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_is_compatible";

  --* Maximum required string length for
  -- * pa_format_info_snprint(). Please note that this value can change
  -- * with any release without warning and without being considered API
  -- * or ABI breakage. You should not use this definition anywhere where
  -- * it might become part of an ABI. \since 1.0  

  --* Make a human-readable string representing the given format. Returns \a s. \since 1.0  
   function pa_format_info_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      f : access constant pa_format_info) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/format.h:140
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_snprint";

  --* Parse a human-readable string of the form generated by
  -- * \a pa_format_info_snprint() into a pa_format_info structure. \since 1.0  

   function pa_format_info_from_string (str : Interfaces.C.Strings.chars_ptr) return access pa_format_info  -- /usr/include/pulse/format.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_from_string";

  --* Utility function to take a \a pa_sample_spec and generate the corresponding
  -- * \a pa_format_info.
  -- *
  -- * Note that if you want the server to choose some of the stream parameters,
  -- * for example the sample rate, so that they match the device parameters, then
  -- * you shouldn't use this function. In order to allow the server to choose
  -- * a parameter value, that parameter must be left unspecified in the
  -- * pa_format_info object, and this function always specifies all parameters. An
  -- * exception is the channel map: if you pass NULL for the channel map, then the
  -- * channel map will be left unspecified, allowing the server to choose it.
  -- *
  -- * \since 2.0  

   function pa_format_info_from_sample_spec (ss : access constant pulse_sample_h.pa_sample_spec; map : access constant pulse_channelmap_h.pa_channel_map) return access pa_format_info  -- /usr/include/pulse/format.h:158
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_from_sample_spec";

  --* Utility function to generate a \a pa_sample_spec and \a pa_channel_map corresponding to a given \a pa_format_info. The
  -- * conversion for PCM formats is straight-forward. For non-PCM formats, if there is a fixed size-time conversion (i.e. all
  -- * IEC61937-encapsulated formats), a "fake" sample spec whose size-time conversion corresponds to this format is provided and
  -- * the channel map argument is ignored. For formats with variable size-time conversion, this function will fail. Returns a
  -- * negative integer if conversion failed and 0 on success. \since 2.0  

   function pa_format_info_to_sample_spec
     (f : access constant pa_format_info;
      ss : access pulse_sample_h.pa_sample_spec;
      map : access pulse_channelmap_h.pa_channel_map) return int  -- /usr/include/pulse/format.h:165
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_to_sample_spec";

  --* Represents the type of value type of a property on a \ref pa_format_info. \since 2.0  
   subtype pa_prop_type_t is int;
   PA_PROP_TYPE_INT : constant pa_prop_type_t := 0;
   PA_PROP_TYPE_INT_RANGE : constant pa_prop_type_t := 1;
   PA_PROP_TYPE_INT_ARRAY : constant pa_prop_type_t := 2;
   PA_PROP_TYPE_STRING : constant pa_prop_type_t := 3;
   PA_PROP_TYPE_STRING_ARRAY : constant pa_prop_type_t := 4;
   PA_PROP_TYPE_INVALID : constant pa_prop_type_t := -1;  -- /usr/include/pulse/format.h:168

  --*< Integer property  
  --*< Integer range property  
  --*< Integer array property  
  --*< String property  
  --*< String array property  
  --*< Represents an invalid type  
  --* \cond fulldocs  
  --* \endcond  
  --* Gets the type of property \a key in a given \ref pa_format_info. \since 2.0  
   function pa_format_info_get_prop_type (f : access constant pa_format_info; key : Interfaces.C.Strings.chars_ptr) return pa_prop_type_t  -- /usr/include/pulse/format.h:198
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_prop_type";

  --* Gets an integer property from the given format info. Returns 0 on success and a negative integer on failure. \since 2.0  
   function pa_format_info_get_prop_int
     (f : access constant pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      v : access int) return int  -- /usr/include/pulse/format.h:201
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_prop_int";

  --* Gets an integer range property from the given format info. Returns 0 on success and a negative integer on failure.
  -- * \since 2.0  

   function pa_format_info_get_prop_int_range
     (f : access constant pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      min : access int;
      max : access int) return int  -- /usr/include/pulse/format.h:204
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_prop_int_range";

  --* Gets an integer array property from the given format info. \a values contains the values and \a n_values contains the
  -- * number of elements. The caller must free \a values using \ref pa_xfree. Returns 0 on success and a negative integer on
  -- * failure. \since 2.0  

   function pa_format_info_get_prop_int_array
     (f : access constant pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      values : System.Address;
      n_values : access int) return int  -- /usr/include/pulse/format.h:208
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_prop_int_array";

  --* Gets a string property from the given format info.  The caller must free the returned string using \ref pa_xfree. Returns
  -- * 0 on success and a negative integer on failure. \since 2.0  

   function pa_format_info_get_prop_string
     (f : access constant pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      v : System.Address) return int  -- /usr/include/pulse/format.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_prop_string";

  --* Gets a string array property from the given format info. \a values contains the values and \a n_values contains
  -- * the number of elements. The caller must free \a values using \ref pa_format_info_free_string_array. Returns 0 on success and
  -- * a negative integer on failure. \since 2.0  

   function pa_format_info_get_prop_string_array
     (f : access constant pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      values : System.Address;
      n_values : access int) return int  -- /usr/include/pulse/format.h:215
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_prop_string_array";

  --* Frees a string array returned by \ref pa_format_info_get_prop_string_array. \since 2.0  
   procedure pa_format_info_free_string_array (values : System.Address; n_values : int)  -- /usr/include/pulse/format.h:218
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_free_string_array";

  --* Gets the sample format stored in the format info. Returns a negative error
  -- * code on failure. If the sample format property is not set at all, returns a
  -- * negative integer. \since 13.0  

   function pa_format_info_get_sample_format (f : access constant pa_format_info; sf : access pulse_sample_h.pa_sample_format_t) return int  -- /usr/include/pulse/format.h:223
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_sample_format";

  --* Gets the sample rate stored in the format info. Returns a negative error
  -- * code on failure. If the sample rate property is not set at all, returns a
  -- * negative integer. \since 13.0  

   function pa_format_info_get_rate (f : access constant pa_format_info; rate : access bits_stdint_uintn_h.uint32_t) return int  -- /usr/include/pulse/format.h:228
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_rate";

  --* Gets the channel count stored in the format info. Returns a negative error
  -- * code on failure. If the channels property is not set at all, returns a
  -- * negative integer. \since 13.0  

   function pa_format_info_get_channels (f : access constant pa_format_info; channels : access bits_stdint_uintn_h.uint8_t) return int  -- /usr/include/pulse/format.h:233
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_channels";

  --* Gets the channel map stored in the format info. Returns a negative error
  -- * code on failure. If the channel map property is not
  -- * set at all, returns a negative integer. \since 13.0  

   function pa_format_info_get_channel_map (f : access constant pa_format_info; map : access pulse_channelmap_h.pa_channel_map) return int  -- /usr/include/pulse/format.h:238
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_get_channel_map";

  --* Sets an integer property on the given format info. \since 1.0  
   procedure pa_format_info_set_prop_int
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      value : int)  -- /usr/include/pulse/format.h:241
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_set_prop_int";

  --* Sets a property with a list of integer values on the given format info. \since 1.0  
   procedure pa_format_info_set_prop_int_array
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      values : access int;
      n_values : int)  -- /usr/include/pulse/format.h:243
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_set_prop_int_array";

  --* Sets a property which can have any value in a given integer range on the given format info. \since 1.0  
   procedure pa_format_info_set_prop_int_range
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      min : int;
      max : int)  -- /usr/include/pulse/format.h:245
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_set_prop_int_range";

  --* Sets a string property on the given format info. \since 1.0  
   procedure pa_format_info_set_prop_string
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings.chars_ptr)  -- /usr/include/pulse/format.h:247
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_set_prop_string";

  --* Sets a property with a list of string values on the given format info. \since 1.0  
   procedure pa_format_info_set_prop_string_array
     (f : access pa_format_info;
      key : Interfaces.C.Strings.chars_ptr;
      values : System.Address;
      n_values : int)  -- /usr/include/pulse/format.h:249
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_set_prop_string_array";

  --* Convenience method to set the sample format as a property on the given
  -- * format.
  -- *
  -- * Note for PCM: If the sample format is left unspecified in the pa_format_info
  -- * object, then the server will select the stream sample format. In that case
  -- * the stream sample format will most likely match the device sample format,
  -- * meaning that sample format conversion will be avoided.
  -- *
  -- * \since 1.0  

   procedure pa_format_info_set_sample_format (f : access pa_format_info; sf : pulse_sample_h.pa_sample_format_t)  -- /usr/include/pulse/format.h:260
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_set_sample_format";

  --* Convenience method to set the sampling rate as a property on the given
  -- * format.
  -- *
  -- * Note for PCM: If the sample rate is left unspecified in the pa_format_info
  -- * object, then the server will select the stream sample rate. In that case the
  -- * stream sample rate will most likely match the device sample rate, meaning
  -- * that sample rate conversion will be avoided.
  -- *
  -- * \since 1.0  

   procedure pa_format_info_set_rate (f : access pa_format_info; rate : int)  -- /usr/include/pulse/format.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_set_rate";

  --* Convenience method to set the number of channels as a property on the given
  -- * format.
  -- *
  -- * Note for PCM: If the channel count is left unspecified in the pa_format_info
  -- * object, then the server will select the stream channel count. In that case
  -- * the stream channel count will most likely match the device channel count,
  -- * meaning that up/downmixing will be avoided.
  -- *
  -- * \since 1.0  

   procedure pa_format_info_set_channels (f : access pa_format_info; channels : int)  -- /usr/include/pulse/format.h:282
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_set_channels";

  --* Convenience method to set the channel map as a property on the given
  -- * format.
  -- *
  -- * Note for PCM: If the channel map is left unspecified in the pa_format_info
  -- * object, then the server will select the stream channel map. In that case the
  -- * stream channel map will most likely match the device channel map, meaning
  -- * that remixing will be avoided.
  -- *
  -- * \since 1.0  

   procedure pa_format_info_set_channel_map (f : access pa_format_info; map : access constant pulse_channelmap_h.pa_channel_map)  -- /usr/include/pulse/format.h:293
   with Import => True, 
        Convention => C, 
        External_Name => "pa_format_info_set_channel_map";

end pulse_format_h;
