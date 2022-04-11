pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_stdint_uintn_h;
with Interfaces.C.Strings;
with stddef_h;
limited with pulse_sample_h;

package pulse_channelmap_h is

   --  unsupported macro: PA_CHANNEL_POSITION_INVALID PA_CHANNEL_POSITION_INVALID
   --  unsupported macro: PA_CHANNEL_POSITION_MONO PA_CHANNEL_POSITION_MONO
   --  unsupported macro: PA_CHANNEL_POSITION_LEFT PA_CHANNEL_POSITION_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_RIGHT PA_CHANNEL_POSITION_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_CENTER PA_CHANNEL_POSITION_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_LEFT PA_CHANNEL_POSITION_FRONT_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_RIGHT PA_CHANNEL_POSITION_FRONT_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_CENTER PA_CHANNEL_POSITION_FRONT_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_REAR_CENTER PA_CHANNEL_POSITION_REAR_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_REAR_LEFT PA_CHANNEL_POSITION_REAR_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_REAR_RIGHT PA_CHANNEL_POSITION_REAR_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_LFE PA_CHANNEL_POSITION_LFE
   --  unsupported macro: PA_CHANNEL_POSITION_SUBWOOFER PA_CHANNEL_POSITION_SUBWOOFER
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_SIDE_LEFT PA_CHANNEL_POSITION_SIDE_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_SIDE_RIGHT PA_CHANNEL_POSITION_SIDE_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_AUX0 PA_CHANNEL_POSITION_AUX0
   --  unsupported macro: PA_CHANNEL_POSITION_AUX1 PA_CHANNEL_POSITION_AUX1
   --  unsupported macro: PA_CHANNEL_POSITION_AUX2 PA_CHANNEL_POSITION_AUX2
   --  unsupported macro: PA_CHANNEL_POSITION_AUX3 PA_CHANNEL_POSITION_AUX3
   --  unsupported macro: PA_CHANNEL_POSITION_AUX4 PA_CHANNEL_POSITION_AUX4
   --  unsupported macro: PA_CHANNEL_POSITION_AUX5 PA_CHANNEL_POSITION_AUX5
   --  unsupported macro: PA_CHANNEL_POSITION_AUX6 PA_CHANNEL_POSITION_AUX6
   --  unsupported macro: PA_CHANNEL_POSITION_AUX7 PA_CHANNEL_POSITION_AUX7
   --  unsupported macro: PA_CHANNEL_POSITION_AUX8 PA_CHANNEL_POSITION_AUX8
   --  unsupported macro: PA_CHANNEL_POSITION_AUX9 PA_CHANNEL_POSITION_AUX9
   --  unsupported macro: PA_CHANNEL_POSITION_AUX10 PA_CHANNEL_POSITION_AUX10
   --  unsupported macro: PA_CHANNEL_POSITION_AUX11 PA_CHANNEL_POSITION_AUX11
   --  unsupported macro: PA_CHANNEL_POSITION_AUX12 PA_CHANNEL_POSITION_AUX12
   --  unsupported macro: PA_CHANNEL_POSITION_AUX13 PA_CHANNEL_POSITION_AUX13
   --  unsupported macro: PA_CHANNEL_POSITION_AUX14 PA_CHANNEL_POSITION_AUX14
   --  unsupported macro: PA_CHANNEL_POSITION_AUX15 PA_CHANNEL_POSITION_AUX15
   --  unsupported macro: PA_CHANNEL_POSITION_AUX16 PA_CHANNEL_POSITION_AUX16
   --  unsupported macro: PA_CHANNEL_POSITION_AUX17 PA_CHANNEL_POSITION_AUX17
   --  unsupported macro: PA_CHANNEL_POSITION_AUX18 PA_CHANNEL_POSITION_AUX18
   --  unsupported macro: PA_CHANNEL_POSITION_AUX19 PA_CHANNEL_POSITION_AUX19
   --  unsupported macro: PA_CHANNEL_POSITION_AUX20 PA_CHANNEL_POSITION_AUX20
   --  unsupported macro: PA_CHANNEL_POSITION_AUX21 PA_CHANNEL_POSITION_AUX21
   --  unsupported macro: PA_CHANNEL_POSITION_AUX22 PA_CHANNEL_POSITION_AUX22
   --  unsupported macro: PA_CHANNEL_POSITION_AUX23 PA_CHANNEL_POSITION_AUX23
   --  unsupported macro: PA_CHANNEL_POSITION_AUX24 PA_CHANNEL_POSITION_AUX24
   --  unsupported macro: PA_CHANNEL_POSITION_AUX25 PA_CHANNEL_POSITION_AUX25
   --  unsupported macro: PA_CHANNEL_POSITION_AUX26 PA_CHANNEL_POSITION_AUX26
   --  unsupported macro: PA_CHANNEL_POSITION_AUX27 PA_CHANNEL_POSITION_AUX27
   --  unsupported macro: PA_CHANNEL_POSITION_AUX28 PA_CHANNEL_POSITION_AUX28
   --  unsupported macro: PA_CHANNEL_POSITION_AUX29 PA_CHANNEL_POSITION_AUX29
   --  unsupported macro: PA_CHANNEL_POSITION_AUX30 PA_CHANNEL_POSITION_AUX30
   --  unsupported macro: PA_CHANNEL_POSITION_AUX31 PA_CHANNEL_POSITION_AUX31
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_CENTER PA_CHANNEL_POSITION_TOP_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_FRONT_LEFT PA_CHANNEL_POSITION_TOP_FRONT_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_FRONT_RIGHT PA_CHANNEL_POSITION_TOP_FRONT_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_FRONT_CENTER PA_CHANNEL_POSITION_TOP_FRONT_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_REAR_LEFT PA_CHANNEL_POSITION_TOP_REAR_LEFT
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_REAR_RIGHT PA_CHANNEL_POSITION_TOP_REAR_RIGHT
   --  unsupported macro: PA_CHANNEL_POSITION_TOP_REAR_CENTER PA_CHANNEL_POSITION_TOP_REAR_CENTER
   --  unsupported macro: PA_CHANNEL_POSITION_MAX PA_CHANNEL_POSITION_MAX
   --  arg-macro: function PA_CHANNEL_POSITION_MASK (f)
   --    return (pa_channel_position_mask_t) (2 ** (f));
   --  unsupported macro: PA_CHANNEL_MAP_AIFF PA_CHANNEL_MAP_AIFF
   --  unsupported macro: PA_CHANNEL_MAP_ALSA PA_CHANNEL_MAP_ALSA
   --  unsupported macro: PA_CHANNEL_MAP_AUX PA_CHANNEL_MAP_AUX
   --  unsupported macro: PA_CHANNEL_MAP_WAVEEX PA_CHANNEL_MAP_WAVEEX
   --  unsupported macro: PA_CHANNEL_MAP_OSS PA_CHANNEL_MAP_OSS
   --  unsupported macro: PA_CHANNEL_MAP_DEF_MAX PA_CHANNEL_MAP_DEF_MAX
   --  unsupported macro: PA_CHANNEL_MAP_DEFAULT PA_CHANNEL_MAP_DEFAULT
   PA_CHANNEL_MAP_SNPRINT_MAX : constant := 336;  --  /usr/include/pulse/channelmap.h:309

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2005-2006 Lennart Poettering
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

  --* \page channelmap Channel Maps
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * Channel maps provide a way to associate channels in a stream with a
  -- * specific speaker position. This relieves applications of having to
  -- * make sure their channel order is identical to the final output.
  -- *
  -- * \section init_sec Initialisation
  -- *
  -- * A channel map consists of an array of \ref pa_channel_position values,
  -- * one for each channel. This array is stored together with a channel count
  -- * in a pa_channel_map structure.
  -- *
  -- * Before filling the structure, the application must initialise it using
  -- * pa_channel_map_init(). There are also a number of convenience functions
  -- * for standard channel mappings:
  -- *
  -- * \li pa_channel_map_init_mono() - Create a channel map with only mono audio.
  -- * \li pa_channel_map_init_stereo() - Create a standard stereo mapping.
  -- * \li pa_channel_map_init_auto() - Create a standard channel map for a specific
  -- *                                  number of channels.
  -- * \li pa_channel_map_init_extend() - Similar to pa_channel_map_init_auto() but
  -- *                                    synthesize a channel map if no predefined
  -- *                                    one is known for the specified number of
  -- *                                    channels.
  -- *
  -- * \section conv_sec Convenience Functions
  -- *
  -- * The library contains a number of convenience functions for dealing with
  -- * channel maps:
  -- *
  -- * \li pa_channel_map_valid() - Tests if a channel map is valid.
  -- * \li pa_channel_map_equal() - Tests if two channel maps are identical.
  -- * \li pa_channel_map_snprint() - Creates a textual description of a channel
  -- *                                map.
  --  

  --* \file
  -- * Constants and routines for channel mapping handling
  -- *
  -- * See also \subpage channelmap
  --  

  --* A list of channel labels  
   subtype pa_channel_position is int;
   PA_CHANNEL_POSITION_INVALID : constant pa_channel_position := -1;
   PA_CHANNEL_POSITION_MONO : constant pa_channel_position := 0;
   PA_CHANNEL_POSITION_FRONT_LEFT : constant pa_channel_position := 1;
   PA_CHANNEL_POSITION_FRONT_RIGHT : constant pa_channel_position := 2;
   PA_CHANNEL_POSITION_FRONT_CENTER : constant pa_channel_position := 3;
   PA_CHANNEL_POSITION_LEFT : constant pa_channel_position := 1;
   PA_CHANNEL_POSITION_RIGHT : constant pa_channel_position := 2;
   PA_CHANNEL_POSITION_CENTER : constant pa_channel_position := 3;
   PA_CHANNEL_POSITION_REAR_CENTER : constant pa_channel_position := 4;
   PA_CHANNEL_POSITION_REAR_LEFT : constant pa_channel_position := 5;
   PA_CHANNEL_POSITION_REAR_RIGHT : constant pa_channel_position := 6;
   PA_CHANNEL_POSITION_LFE : constant pa_channel_position := 7;
   PA_CHANNEL_POSITION_SUBWOOFER : constant pa_channel_position := 7;
   PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER : constant pa_channel_position := 8;
   PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER : constant pa_channel_position := 9;
   PA_CHANNEL_POSITION_SIDE_LEFT : constant pa_channel_position := 10;
   PA_CHANNEL_POSITION_SIDE_RIGHT : constant pa_channel_position := 11;
   PA_CHANNEL_POSITION_AUX0 : constant pa_channel_position := 12;
   PA_CHANNEL_POSITION_AUX1 : constant pa_channel_position := 13;
   PA_CHANNEL_POSITION_AUX2 : constant pa_channel_position := 14;
   PA_CHANNEL_POSITION_AUX3 : constant pa_channel_position := 15;
   PA_CHANNEL_POSITION_AUX4 : constant pa_channel_position := 16;
   PA_CHANNEL_POSITION_AUX5 : constant pa_channel_position := 17;
   PA_CHANNEL_POSITION_AUX6 : constant pa_channel_position := 18;
   PA_CHANNEL_POSITION_AUX7 : constant pa_channel_position := 19;
   PA_CHANNEL_POSITION_AUX8 : constant pa_channel_position := 20;
   PA_CHANNEL_POSITION_AUX9 : constant pa_channel_position := 21;
   PA_CHANNEL_POSITION_AUX10 : constant pa_channel_position := 22;
   PA_CHANNEL_POSITION_AUX11 : constant pa_channel_position := 23;
   PA_CHANNEL_POSITION_AUX12 : constant pa_channel_position := 24;
   PA_CHANNEL_POSITION_AUX13 : constant pa_channel_position := 25;
   PA_CHANNEL_POSITION_AUX14 : constant pa_channel_position := 26;
   PA_CHANNEL_POSITION_AUX15 : constant pa_channel_position := 27;
   PA_CHANNEL_POSITION_AUX16 : constant pa_channel_position := 28;
   PA_CHANNEL_POSITION_AUX17 : constant pa_channel_position := 29;
   PA_CHANNEL_POSITION_AUX18 : constant pa_channel_position := 30;
   PA_CHANNEL_POSITION_AUX19 : constant pa_channel_position := 31;
   PA_CHANNEL_POSITION_AUX20 : constant pa_channel_position := 32;
   PA_CHANNEL_POSITION_AUX21 : constant pa_channel_position := 33;
   PA_CHANNEL_POSITION_AUX22 : constant pa_channel_position := 34;
   PA_CHANNEL_POSITION_AUX23 : constant pa_channel_position := 35;
   PA_CHANNEL_POSITION_AUX24 : constant pa_channel_position := 36;
   PA_CHANNEL_POSITION_AUX25 : constant pa_channel_position := 37;
   PA_CHANNEL_POSITION_AUX26 : constant pa_channel_position := 38;
   PA_CHANNEL_POSITION_AUX27 : constant pa_channel_position := 39;
   PA_CHANNEL_POSITION_AUX28 : constant pa_channel_position := 40;
   PA_CHANNEL_POSITION_AUX29 : constant pa_channel_position := 41;
   PA_CHANNEL_POSITION_AUX30 : constant pa_channel_position := 42;
   PA_CHANNEL_POSITION_AUX31 : constant pa_channel_position := 43;
   PA_CHANNEL_POSITION_TOP_CENTER : constant pa_channel_position := 44;
   PA_CHANNEL_POSITION_TOP_FRONT_LEFT : constant pa_channel_position := 45;
   PA_CHANNEL_POSITION_TOP_FRONT_RIGHT : constant pa_channel_position := 46;
   PA_CHANNEL_POSITION_TOP_FRONT_CENTER : constant pa_channel_position := 47;
   PA_CHANNEL_POSITION_TOP_REAR_LEFT : constant pa_channel_position := 48;
   PA_CHANNEL_POSITION_TOP_REAR_RIGHT : constant pa_channel_position := 49;
   PA_CHANNEL_POSITION_TOP_REAR_CENTER : constant pa_channel_position := 50;
   PA_CHANNEL_POSITION_MAX : constant pa_channel_position := 51;  -- /usr/include/pulse/channelmap.h:76

  --*< Apple, Dolby call this 'Left'  
  --*< Apple, Dolby call this 'Right'  
  --*< Apple, Dolby call this 'Center'  
  --* \cond fulldocs  
  --* \endcond  
  --*< Microsoft calls this 'Back Center', Apple calls this 'Center Surround', Dolby calls this 'Surround Rear Center'  
  --*< Microsoft calls this 'Back Left', Apple calls this 'Left Surround' (!), Dolby calls this 'Surround Rear Left'   
  --*< Microsoft calls this 'Back Right', Apple calls this 'Right Surround' (!), Dolby calls this 'Surround Rear Right'   
  --*< Microsoft calls this 'Low Frequency', Apple calls this 'LFEScreen'  
  --* \cond fulldocs  
  --* \endcond  
  --*< Apple, Dolby call this 'Left Center'  
  --*< Apple, Dolby call this 'Right Center  
  --*< Apple calls this 'Left Surround Direct', Dolby calls this 'Surround Left' (!)  
  --*< Apple calls this 'Right Surround Direct', Dolby calls this 'Surround Right' (!)  
  --*< Apple calls this 'Top Center Surround'  
  --*< Apple calls this 'Vertical Height Left'  
  --*< Apple calls this 'Vertical Height Right'  
  --*< Apple calls this 'Vertical Height Center'  
  --*< Microsoft and Apple call this 'Top Back Left'  
  --*< Microsoft and Apple call this 'Top Back Right'  
  --*< Microsoft and Apple call this 'Top Back Center'  
   subtype pa_channel_position_t is pa_channel_position;  -- /usr/include/pulse/channelmap.h:149

  --* \cond fulldocs  
  --* \endcond  
  --* A mask of channel positions. \since 0.9.16  
   subtype pa_channel_position_mask_t is bits_stdint_uintn_h.uint64_t;  -- /usr/include/pulse/channelmap.h:212

  --* Makes a bit mask from a channel position. \since 0.9.16  
  --* A list of channel mapping definitions for pa_channel_map_init_auto()  
   subtype pa_channel_map_def is unsigned;
   PA_CHANNEL_MAP_AIFF : constant pa_channel_map_def := 0;
   PA_CHANNEL_MAP_ALSA : constant pa_channel_map_def := 1;
   PA_CHANNEL_MAP_AUX : constant pa_channel_map_def := 2;
   PA_CHANNEL_MAP_WAVEEX : constant pa_channel_map_def := 3;
   PA_CHANNEL_MAP_OSS : constant pa_channel_map_def := 4;
   PA_CHANNEL_MAP_DEF_MAX : constant pa_channel_map_def := 5;
   PA_CHANNEL_MAP_DEFAULT : constant pa_channel_map_def := 0;  -- /usr/include/pulse/channelmap.h:218

  --*< The mapping from RFC3551, which is based on AIFF-C  
  --* \cond fulldocs  
  --*< The default mapping used by ALSA. This mapping is probably
  --     * not too useful since ALSA's default channel mapping depends on
  --     * the device string used.  

  --* \endcond  
  --*< Only aux channels  
  --*< Microsoft's WAVEFORMATEXTENSIBLE mapping. This mapping works
  --     * as if all LSBs of dwChannelMask are set.   

  --* \cond fulldocs  
  --*< The default channel mapping used by OSS as defined in the OSS
  --     * 4.0 API specs. This mapping is probably not too useful since
  --     * the OSS API has changed in this respect and no longer knows a
  --     * default channel mapping based on the number of channels.  

  --* \endcond  
  --*< Upper limit of valid channel mapping definitions  
  --*< The default channel map  
   subtype pa_channel_map_def_t is pa_channel_map_def;  -- /usr/include/pulse/channelmap.h:249

  --* \cond fulldocs  
  --* \endcond  
  --* A channel map which can be used to attach labels to specific
  -- * channels of a stream. These values are relevant for conversion and
  -- * mixing of streams  

   type anon1281_array1282 is array (0 .. 31) of aliased pa_channel_position_t;
   type pa_channel_map is record
      channels : aliased bits_stdint_uintn_h.uint8_t;  -- /usr/include/pulse/channelmap.h:265
      map : aliased anon1281_array1282;  -- /usr/include/pulse/channelmap.h:268
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/channelmap.h:264

  --*< Number of channels mapped  
  --*< Channel labels  
  --* Initialize the specified channel map and return a pointer to
  -- * it. The channel map will have a defined state but
  -- * pa_channel_map_valid() will fail for it.  

   function pa_channel_map_init (m : access pa_channel_map) return access pa_channel_map  -- /usr/include/pulse/channelmap.h:275
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_init";

  --* Initialize the specified channel map for monaural audio and return a pointer to it  
   function pa_channel_map_init_mono (m : access pa_channel_map) return access pa_channel_map  -- /usr/include/pulse/channelmap.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_init_mono";

  --* Initialize the specified channel map for stereophonic audio and return a pointer to it  
   function pa_channel_map_init_stereo (m : access pa_channel_map) return access pa_channel_map  -- /usr/include/pulse/channelmap.h:281
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_init_stereo";

  --* Initialize the specified channel map for the specified number of
  -- * channels using default labels and return a pointer to it. This call
  -- * will fail (return NULL) if there is no default channel map known for this
  -- * specific number of channels and mapping.  

   function pa_channel_map_init_auto
     (m : access pa_channel_map;
      channels : unsigned;
      def : pa_channel_map_def_t) return access pa_channel_map  -- /usr/include/pulse/channelmap.h:287
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_init_auto";

  --* Similar to pa_channel_map_init_auto() but instead of failing if no
  -- * default mapping is known with the specified parameters it will
  -- * synthesize a mapping based on a known mapping with fewer channels
  -- * and fill up the rest with AUX0...AUX31 channels  \since 0.9.11  

   function pa_channel_map_init_extend
     (m : access pa_channel_map;
      channels : unsigned;
      def : pa_channel_map_def_t) return access pa_channel_map  -- /usr/include/pulse/channelmap.h:293
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_init_extend";

  --* Return a text label for the specified channel position  
   function pa_channel_position_to_string (pos : pa_channel_position_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/channelmap.h:296
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_position_to_string";

  --* The inverse of pa_channel_position_to_string(). \since 0.9.16  
   function pa_channel_position_from_string (s : Interfaces.C.Strings.chars_ptr) return pa_channel_position_t  -- /usr/include/pulse/channelmap.h:299
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_position_from_string";

  --* Return a human readable text label for the specified channel position. \since 0.9.7  
   function pa_channel_position_to_pretty_string (pos : pa_channel_position_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/channelmap.h:302
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_position_to_pretty_string";

  --* The maximum length of strings returned by
  -- * pa_channel_map_snprint(). Please note that this value can change
  -- * with any release without warning and without being considered API
  -- * or ABI breakage. You should not use this definition anywhere where
  -- * it might become part of an ABI.  

  --* Make a human readable string from the specified channel map. Returns \a s.  
   function pa_channel_map_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      map : access constant pa_channel_map) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/channelmap.h:312
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_snprint";

  --* Parse a channel position list or well-known mapping name into a
  -- * channel map structure. This turns the output of
  -- * pa_channel_map_snprint() and pa_channel_map_to_name() back into a
  -- * pa_channel_map  

   function pa_channel_map_parse (map : access pa_channel_map; s : Interfaces.C.Strings.chars_ptr) return access pa_channel_map  -- /usr/include/pulse/channelmap.h:318
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_parse";

  --* Compare two channel maps. Return 1 if both match.  
   function pa_channel_map_equal (a : access constant pa_channel_map; b : access constant pa_channel_map) return int  -- /usr/include/pulse/channelmap.h:321
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_equal";

  --* Return non-zero if the specified channel map is considered valid  
   function pa_channel_map_valid (map : access constant pa_channel_map) return int  -- /usr/include/pulse/channelmap.h:324
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_valid";

  --* Return non-zero if the specified channel map is compatible with
  -- * the specified sample spec. \since 0.9.12  

   function pa_channel_map_compatible (map : access constant pa_channel_map; ss : access constant pulse_sample_h.pa_sample_spec) return int  -- /usr/include/pulse/channelmap.h:328
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_compatible";

  --* Returns non-zero if every channel defined in b is also defined in a. \since 0.9.15  
   function pa_channel_map_superset (a : access constant pa_channel_map; b : access constant pa_channel_map) return int  -- /usr/include/pulse/channelmap.h:331
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_superset";

  --* Returns non-zero if it makes sense to apply a volume 'balance'
  -- * with this mapping, i.e.\ if there are left/right channels
  -- * available. \since 0.9.15  

   function pa_channel_map_can_balance (map : access constant pa_channel_map) return int  -- /usr/include/pulse/channelmap.h:336
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_can_balance";

  --* Returns non-zero if it makes sense to apply a volume 'fade'
  -- * (i.e.\ 'balance' between front and rear) with this mapping, i.e.\ if
  -- * there are front/rear channels available. \since 0.9.15  

   function pa_channel_map_can_fade (map : access constant pa_channel_map) return int  -- /usr/include/pulse/channelmap.h:341
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_can_fade";

  --* Returns non-zero if it makes sense to apply a volume 'lfe balance'
  -- * (i.e.\ 'balance' between LFE and non-LFE channels) with this mapping,
  -- *  i.e.\ if there are LFE and non-LFE channels available. \since 8.0  

   function pa_channel_map_can_lfe_balance (map : access constant pa_channel_map) return int  -- /usr/include/pulse/channelmap.h:346
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_can_lfe_balance";

  --* Tries to find a well-known channel mapping name for this channel
  -- * mapping, i.e.\ "stereo", "surround-71" and so on. If the channel
  -- * mapping is unknown NULL will be returned. This name can be parsed
  -- * with pa_channel_map_parse() \since 0.9.15  

   function pa_channel_map_to_name (map : access constant pa_channel_map) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/channelmap.h:352
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_to_name";

  --* Tries to find a human readable text label for this channel
  --mapping, i.e.\ "Stereo", "Surround 7.1" and so on. If the channel
  --mapping is unknown NULL will be returned. \since 0.9.15  

   function pa_channel_map_to_pretty_name (map : access constant pa_channel_map) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/channelmap.h:357
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_to_pretty_name";

  --* Returns non-zero if the specified channel position is available at
  -- * least once in the channel map. \since 0.9.16  

   function pa_channel_map_has_position (map : access constant pa_channel_map; p : pa_channel_position_t) return int  -- /usr/include/pulse/channelmap.h:361
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_has_position";

  --* Generates a bit mask from a channel map. \since 0.9.16  
   function pa_channel_map_mask (map : access constant pa_channel_map) return pa_channel_position_mask_t  -- /usr/include/pulse/channelmap.h:364
   with Import => True, 
        Convention => C, 
        External_Name => "pa_channel_map_mask";

end pulse_channelmap_h;
