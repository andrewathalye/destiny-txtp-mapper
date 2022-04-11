pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_stdint_uintn_h;
with Interfaces.C.Strings;
with stddef_h;
with pulse_channelmap_h;
limited with pulse_sample_h;

package pulse_volume_h is

   --  unsupported macro: PA_VOLUME_NORM ((pa_volume_t) 0x10000U)
   --  unsupported macro: PA_VOLUME_MUTED ((pa_volume_t) 0U)
   --  unsupported macro: PA_VOLUME_MAX ((pa_volume_t) UINT32_MAX/2)
   --  unsupported macro: PA_VOLUME_UI_MAX (pa_sw_volume_from_dB(+11.0))
   --  unsupported macro: PA_VOLUME_INVALID ((pa_volume_t) UINT32_MAX)
   --  arg-macro: function PA_VOLUME_IS_VALID (v)
   --    return (v) <= PA_VOLUME_MAX;
   --  arg-macro: function PA_CLAMP_VOLUME (v)
   --    return PA_CLAMP_UNLIKELY((v), PA_VOLUME_MUTED, PA_VOLUME_MAX);
   --  arg-macro: procedure pa_cvolume_reset (a, n)
   --    pa_cvolume_set((a), (n), PA_VOLUME_NORM)
   --  arg-macro: procedure pa_cvolume_mute (a, n)
   --    pa_cvolume_set((a), (n), PA_VOLUME_MUTED)
   PA_CVOLUME_SNPRINT_MAX : constant := 320;  --  /usr/include/pulse/volume.h:174

   PA_SW_CVOLUME_SNPRINT_DB_MAX : constant := 448;  --  /usr/include/pulse/volume.h:184

   PA_CVOLUME_SNPRINT_VERBOSE_MAX : constant := 1984;  --  /usr/include/pulse/volume.h:193

   PA_VOLUME_SNPRINT_MAX : constant := 10;  --  /usr/include/pulse/volume.h:206

   PA_SW_VOLUME_SNPRINT_DB_MAX : constant := 11;  --  /usr/include/pulse/volume.h:216

   PA_VOLUME_SNPRINT_VERBOSE_MAX : constant := 35;  --  /usr/include/pulse/volume.h:225
   --  arg-macro: procedure pa_cvolume_is_muted (a)
   --    pa_cvolume_channels_equal_to((a), PA_VOLUME_MUTED)
   --  arg-macro: procedure pa_cvolume_is_norm (a)
   --    pa_cvolume_channels_equal_to((a), PA_VOLUME_NORM)
   --  unsupported macro: PA_DECIBEL_MININFTY ((double) -200.0)

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

  --* \page volume Volume Control
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * Sinks, sources, sink inputs, source outputs and samples can all have their
  -- * own volumes. To deal with these, The PulseAudio library contains a number of
  -- * functions that ease handling.
  -- *
  -- * The basic volume type in PulseAudio is the \ref pa_volume_t type. Most of
  -- * the time, applications will use the aggregated pa_cvolume structure that
  -- * can store the volume of all channels at once.
  -- *
  -- * Volumes commonly span between muted (0%), and normal (100%). It is possible
  -- * to set volumes to higher than 100%, but clipping might occur.
  -- *
  -- * There is no single well-defined meaning attached to the 100% volume for a
  -- * sink input. In fact, it depends on the server configuration. With flat
  -- * volumes enabled, it means the maximum volume that the sound hardware is
  -- * capable of, which is usually so high that you absolutely must not set sink
  -- * input volume to 100% unless the the user explicitly requests that (note that
  -- * usually you shouldn't set the volume anyway if the user doesn't explicitly
  -- * request it, instead, let PulseAudio decide the volume for the sink input).
  -- * With flat volumes disabled the sink input volume is relative to the sink
  -- * volume, so 100% sink input volume means that the sink input is played at the
  -- * current sink volume level. In this case 100% is often a good default volume
  -- * for a sink input, although you still should let PulseAudio decide the
  -- * default volume. It is possible to figure out whether flat volume mode is in
  -- * effect for a given sink by calling pa_context_get_sink_info_by_name().
  -- *
  -- * \section calc_sec Calculations
  -- *
  -- * The volumes in PulseAudio are cubic in nature and applications shouldn't
  -- * perform calculations with them directly. Instead, they should be converted
  -- * to and from either dB or a linear scale:
  -- *
  -- * \li dB - pa_sw_volume_from_dB() / pa_sw_volume_to_dB()
  -- * \li Linear - pa_sw_volume_from_linear() / pa_sw_volume_to_linear()
  -- *
  -- * For simple multiplication, pa_sw_volume_multiply() and
  -- * pa_sw_cvolume_multiply() can be used.
  -- *
  -- * It's often unknown what scale hardware volumes relate to. Don't use the
  -- * above functions on sink and source volumes, unless the sink or source in
  -- * question has the PA_SINK_DECIBEL_VOLUME or PA_SOURCE_DECIBEL_VOLUME flag
  -- * set. The conversion functions are rarely needed anyway, most of the time
  -- * it's sufficient to treat all volumes as opaque with a range from
  -- * PA_VOLUME_MUTED (0%) to PA_VOLUME_NORM (100%).
  -- *
  -- * \section conv_sec Convenience Functions
  -- *
  -- * To handle the pa_cvolume structure, the PulseAudio library provides a
  -- * number of convenience functions:
  -- *
  -- * \li pa_cvolume_valid() - Tests if a pa_cvolume structure is valid.
  -- * \li pa_cvolume_equal() - Tests if two pa_cvolume structures are identical.
  -- * \li pa_cvolume_channels_equal_to() - Tests if all channels of a pa_cvolume
  -- *                             structure have a given volume.
  -- * \li pa_cvolume_is_muted() - Tests if all channels of a pa_cvolume
  -- *                             structure are muted.
  -- * \li pa_cvolume_is_norm() - Tests if all channels of a pa_cvolume structure
  -- *                            are at a normal volume.
  -- * \li pa_cvolume_set() - Set the first n channels of a pa_cvolume structure to
  -- *                        a certain volume.
  -- * \li pa_cvolume_reset() - Set the first n channels of a pa_cvolume structure
  -- *                          to a normal volume.
  -- * \li pa_cvolume_mute() - Set the first n channels of a pa_cvolume structure
  -- *                         to a muted volume.
  -- * \li pa_cvolume_avg() - Return the average volume of all channels.
  -- * \li pa_cvolume_snprint() - Pretty print a pa_cvolume structure.
  --  

  --* \file
  -- * Constants and routines for volume handling
  -- *
  -- * See also \subpage volume
  --  

  --* Volume specification:
  -- *  PA_VOLUME_MUTED: silence;
  -- * < PA_VOLUME_NORM: decreased volume;
  -- *   PA_VOLUME_NORM: normal volume;
  -- * > PA_VOLUME_NORM: increased volume  

   subtype pa_volume_t is bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/volume.h:117

  --* Normal volume (100%, 0 dB)  
  --* Muted (minimal valid) volume (0%, -inf dB)  
  --* Maximum valid volume we can store. \since 0.9.15  
  --* Recommended maximum volume to show in user facing UIs.
  -- * Note: UIs should deal gracefully with volumes greater than this value
  -- * and not cause feedback loops etc. - i.e. if the volume is more than
  -- * this, the UI should not limit it and push the limited value back to
  -- * the server. \since 0.9.23  

  --* Special 'invalid' volume. \since 0.9.16  
  --* Check if volume is valid. \since 1.0  
  --* Clamp volume to the permitted range. \since 1.0  
  --* A structure encapsulating a per-channel volume  
  --*< Number of channels  
   type anon1719_array1720 is array (0 .. 31) of aliased pa_volume_t;
   type pa_cvolume is record
      channels : aliased bits_stdint_uintn_h.uint8_t;  -- /usr/include/pulse/volume.h:146
      values : aliased anon1719_array1720;  -- /usr/include/pulse/volume.h:147
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/volume.h:145

  --*< Per-channel volume  
  --* Return non-zero when *a == *b, checking that both a and b
  -- * have the same number of channels and that the volumes of
  -- * channels in a equal those in b.  

   function pa_cvolume_equal (a : access constant pa_cvolume; b : access constant pa_cvolume) return int  -- /usr/include/pulse/volume.h:153
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_equal";

  --* Initialize the specified volume and return a pointer to
  -- * it. The sample spec will have a defined state but
  -- * pa_cvolume_valid() will fail for it. \since 0.9.13  

   function pa_cvolume_init (a : access pa_cvolume) return access pa_cvolume  -- /usr/include/pulse/volume.h:158
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_init";

  --* Set the volume of the first n channels to PA_VOLUME_NORM  
  --* Set the volume of the first n channels to PA_VOLUME_MUTED  
  --* Set the volume of the specified number of channels to the volume v  
   function pa_cvolume_set
     (a : access pa_cvolume;
      channels : unsigned;
      v : pa_volume_t) return access pa_cvolume  -- /usr/include/pulse/volume.h:167
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_set";

  --* Maximum length of the strings returned by
  -- * pa_cvolume_snprint(). Please note that this value can change with
  -- * any release without warning and without being considered API or ABI
  -- * breakage. You should not use this definition anywhere where it
  -- * might become part of an ABI. 

  --* Pretty print a volume structure. Returns \a s.  
   function pa_cvolume_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      c : access constant pa_cvolume) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/volume.h:177
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_snprint";

  --* Maximum length of the strings returned by
  -- * pa_sw_cvolume_snprint_dB(). Please note that this value can change with
  -- * any release without warning and without being considered API or ABI
  -- * breakage. You should not use this definition anywhere where it
  -- * might become part of an ABI. \since 0.9.13  

  --* Pretty print a volume structure, showing dB values. Returns \a s. \since 0.9.13  
   function pa_sw_cvolume_snprint_dB
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      c : access constant pa_cvolume) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/volume.h:187
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_cvolume_snprint_dB";

  --* Maximum length of the strings returned by pa_cvolume_snprint_verbose().
  -- * Please note that this value can change with any release without warning and
  -- * without being considered API or ABI breakage. You should not use this
  -- * definition anywhere where it might become part of an ABI. \since 5.0  

  --* Pretty print a volume structure in a verbose way. The volume for each
  -- * channel is printed in several formats: the raw pa_volume_t value,
  -- * percentage, and if print_dB is non-zero, also the dB value. If map is not
  -- * NULL, the channel names will be printed. Returns \a s. \since 5.0  

   function pa_cvolume_snprint_verbose
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      c : access constant pa_cvolume;
      map : access constant pulse_channelmap_h.pa_channel_map;
      print_dB : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/volume.h:199
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_snprint_verbose";

  --* Maximum length of the strings returned by
  -- * pa_volume_snprint(). Please note that this value can change with
  -- * any release without warning and without being considered API or ABI
  -- * breakage. You should not use this definition anywhere where it
  -- * might become part of an ABI. \since 0.9.15  

  --* Pretty print a volume. Returns \a s. \since 0.9.15  
   function pa_volume_snprint
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      v : pa_volume_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/volume.h:209
   with Import => True, 
        Convention => C, 
        External_Name => "pa_volume_snprint";

  --* Maximum length of the strings returned by
  -- * pa_sw_volume_snprint_dB(). Please note that this value can change with
  -- * any release without warning and without being considered API or ABI
  -- * breakage. You should not use this definition anywhere where it
  -- * might become part of an ABI. \since 0.9.15  

  --* Pretty print a volume but show dB values. Returns \a s. \since 0.9.15  
   function pa_sw_volume_snprint_dB
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      v : pa_volume_t) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/volume.h:219
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_volume_snprint_dB";

  --* Maximum length of the strings returned by pa_volume_snprint_verbose().
  -- * Please note that this value can change with any release without warning and
  -- * withou being considered API or ABI breakage. You should not use this
  -- * definition anywhere where it might become part of an ABI. \since 5.0  

  --* Pretty print a volume in a verbose way. The volume is printed in several
  -- * formats: the raw pa_volume_t value, percentage, and if print_dB is non-zero,
  -- * also the dB value. Returns \a s. \since 5.0  

   function pa_volume_snprint_verbose
     (s : Interfaces.C.Strings.chars_ptr;
      l : stddef_h.size_t;
      v : pa_volume_t;
      print_dB : int) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/volume.h:230
   with Import => True, 
        Convention => C, 
        External_Name => "pa_volume_snprint_verbose";

  --* Return the average volume of all channels  
   function pa_cvolume_avg (a : access constant pa_cvolume) return pa_volume_t  -- /usr/include/pulse/volume.h:233
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_avg";

  --* Return the average volume of all channels that are included in the
  -- * specified channel map with the specified channel position mask. If
  -- * cm is NULL this call is identical to pa_cvolume_avg(). If no
  -- * channel is selected the returned value will be
  -- * PA_VOLUME_MUTED. \since 0.9.16  

   function pa_cvolume_avg_mask
     (a : access constant pa_cvolume;
      cm : access constant pulse_channelmap_h.pa_channel_map;
      mask : pulse_channelmap_h.pa_channel_position_mask_t) return pa_volume_t  -- /usr/include/pulse/volume.h:240
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_avg_mask";

  --* Return the maximum volume of all channels. \since 0.9.12  
   function pa_cvolume_max (a : access constant pa_cvolume) return pa_volume_t  -- /usr/include/pulse/volume.h:243
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_max";

  --* Return the maximum volume of all channels that are included in the
  -- * specified channel map with the specified channel position mask. If
  -- * cm is NULL this call is identical to pa_cvolume_max(). If no
  -- * channel is selected the returned value will be PA_VOLUME_MUTED.
  -- * \since 0.9.16  

   function pa_cvolume_max_mask
     (a : access constant pa_cvolume;
      cm : access constant pulse_channelmap_h.pa_channel_map;
      mask : pulse_channelmap_h.pa_channel_position_mask_t) return pa_volume_t  -- /usr/include/pulse/volume.h:250
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_max_mask";

  --* Return the minimum volume of all channels. \since 0.9.16  
   function pa_cvolume_min (a : access constant pa_cvolume) return pa_volume_t  -- /usr/include/pulse/volume.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_min";

  --* Return the minimum volume of all channels that are included in the
  -- * specified channel map with the specified channel position mask. If
  -- * cm is NULL this call is identical to pa_cvolume_min(). If no
  -- * channel is selected the returned value will be PA_VOLUME_MUTED.
  -- * \since 0.9.16  

   function pa_cvolume_min_mask
     (a : access constant pa_cvolume;
      cm : access constant pulse_channelmap_h.pa_channel_map;
      mask : pulse_channelmap_h.pa_channel_position_mask_t) return pa_volume_t  -- /usr/include/pulse/volume.h:260
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_min_mask";

  --* Return non-zero when the passed cvolume structure is valid  
   function pa_cvolume_valid (v : access constant pa_cvolume) return int  -- /usr/include/pulse/volume.h:263
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_valid";

  --* Return non-zero if the volume of all channels is equal to the specified value  
   function pa_cvolume_channels_equal_to (a : access constant pa_cvolume; v : pa_volume_t) return int  -- /usr/include/pulse/volume.h:266
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_channels_equal_to";

  --* Return 1 if the specified volume has all channels muted  
  --* Return 1 if the specified volume has all channels on normal level  
  --* Multiply two volume specifications, return the result. This uses
  -- * PA_VOLUME_NORM as neutral element of multiplication. This is only
  -- * valid for software volumes!  

   function pa_sw_volume_multiply (a : pa_volume_t; b : pa_volume_t) return pa_volume_t  -- /usr/include/pulse/volume.h:277
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_volume_multiply";

  --* Multiply two per-channel volumes and return the result in
  -- * *dest. This is only valid for software volumes! a, b and dest may
  -- * point to the same structure. Returns dest, or NULL on error.  

   function pa_sw_cvolume_multiply
     (dest : access pa_cvolume;
      a : access constant pa_cvolume;
      b : access constant pa_cvolume) return access pa_cvolume  -- /usr/include/pulse/volume.h:282
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_cvolume_multiply";

  --* Multiply a per-channel volume with a scalar volume and return the
  -- * result in *dest. This is only valid for software volumes! a
  -- * and dest may point to the same structure. Returns dest, or NULL on error.
  -- * \since 0.9.16  

   function pa_sw_cvolume_multiply_scalar
     (dest : access pa_cvolume;
      a : access constant pa_cvolume;
      b : pa_volume_t) return access pa_cvolume  -- /usr/include/pulse/volume.h:288
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_cvolume_multiply_scalar";

  --* Divide two volume specifications, return the result. This uses
  -- * PA_VOLUME_NORM as neutral element of division. This is only valid
  -- * for software volumes! If a division by zero is tried the result
  -- * will be 0. \since 0.9.13  

   function pa_sw_volume_divide (a : pa_volume_t; b : pa_volume_t) return pa_volume_t  -- /usr/include/pulse/volume.h:294
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_volume_divide";

  --* Divide two per-channel volumes and return the result in
  -- * *dest. This is only valid for software volumes! a, b
  -- * and dest may point to the same structure. Returns dest,
  -- * or NULL on error. \since 0.9.13  

   function pa_sw_cvolume_divide
     (dest : access pa_cvolume;
      a : access constant pa_cvolume;
      b : access constant pa_cvolume) return access pa_cvolume  -- /usr/include/pulse/volume.h:300
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_cvolume_divide";

  --* Divide a per-channel volume by a scalar volume and return the
  -- * result in *dest. This is only valid for software volumes! a
  -- * and dest may point to the same structure. Returns dest,
  -- * or NULL on error. \since 0.9.16  

   function pa_sw_cvolume_divide_scalar
     (dest : access pa_cvolume;
      a : access constant pa_cvolume;
      b : pa_volume_t) return access pa_cvolume  -- /usr/include/pulse/volume.h:306
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_cvolume_divide_scalar";

  --* Convert a decibel value to a volume (amplitude, not power). This is only valid for software volumes!  
   function pa_sw_volume_from_dB (f : double) return pa_volume_t  -- /usr/include/pulse/volume.h:309
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_volume_from_dB";

  --* Convert a volume to a decibel value (amplitude, not power). This is only valid for software volumes!  
   function pa_sw_volume_to_dB (v : pa_volume_t) return double  -- /usr/include/pulse/volume.h:312
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_volume_to_dB";

  --* Convert a linear factor to a volume. 0.0 and less is muted while
  -- * 1.0 is PA_VOLUME_NORM. This is only valid for software volumes!  

   function pa_sw_volume_from_linear (v : double) return pa_volume_t  -- /usr/include/pulse/volume.h:316
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_volume_from_linear";

  --* Convert a volume to a linear factor. This is only valid for software volumes!  
   function pa_sw_volume_to_linear (v : pa_volume_t) return double  -- /usr/include/pulse/volume.h:319
   with Import => True, 
        Convention => C, 
        External_Name => "pa_sw_volume_to_linear";

  --* This floor value is used as minus infinity when using pa_sw_volume_to_dB() / pa_sw_volume_from_dB().  
  --* Remap a volume from one channel mapping to a different channel mapping.
  -- * Returns \a v. \since 0.9.12  

   function pa_cvolume_remap
     (v : access pa_cvolume;
      from : access constant pulse_channelmap_h.pa_channel_map;
      to : access constant pulse_channelmap_h.pa_channel_map) return access pa_cvolume  -- /usr/include/pulse/volume.h:330
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_remap";

  --* Return non-zero if the specified volume is compatible with the
  -- * specified sample spec. \since 0.9.13  

   function pa_cvolume_compatible (v : access constant pa_cvolume; ss : access constant pulse_sample_h.pa_sample_spec) return int  -- /usr/include/pulse/volume.h:334
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_compatible";

  --* Return non-zero if the specified volume is compatible with the
  -- * specified sample spec. \since 0.9.15  

   function pa_cvolume_compatible_with_channel_map (v : access constant pa_cvolume; cm : access constant pulse_channelmap_h.pa_channel_map) return int  -- /usr/include/pulse/volume.h:338
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_compatible_with_channel_map";

  --* Calculate a 'balance' value for the specified volume with the
  -- * specified channel map. The return value will range from -1.0f
  -- * (left) to +1.0f (right). If no balance value is applicable to this
  -- * channel map the return value will always be 0.0f. See
  -- * pa_channel_map_can_balance(). \since 0.9.15  

   function pa_cvolume_get_balance (v : access constant pa_cvolume; map : access constant pulse_channelmap_h.pa_channel_map) return float  -- /usr/include/pulse/volume.h:345
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_get_balance";

  --* Adjust the 'balance' value for the specified volume with the
  -- * specified channel map. v will be modified in place and
  -- * returned. The balance is a value between -1.0f and +1.0f. This
  -- * operation might not be reversible! Also, after this call
  -- * pa_cvolume_get_balance() is not guaranteed to actually return the
  -- * requested balance value (e.g. when the input volume was zero anyway for
  -- * all channels). If no balance value is applicable to
  -- * this channel map the volume will not be modified. See
  -- * pa_channel_map_can_balance(). Will return NULL on error. \since 0.9.15  

   function pa_cvolume_set_balance
     (v : access pa_cvolume;
      map : access constant pulse_channelmap_h.pa_channel_map;
      new_balance : float) return access pa_cvolume  -- /usr/include/pulse/volume.h:356
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_set_balance";

  --* Calculate a 'fade' value (i.e.\ 'balance' between front and rear)
  -- * for the specified volume with the specified channel map. The return
  -- * value will range from -1.0f (rear) to +1.0f (left). If no fade
  -- * value is applicable to this channel map the return value will
  -- * always be 0.0f. See pa_channel_map_can_fade(). \since 0.9.15  

   function pa_cvolume_get_fade (v : access constant pa_cvolume; map : access constant pulse_channelmap_h.pa_channel_map) return float  -- /usr/include/pulse/volume.h:363
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_get_fade";

  --* Adjust the 'fade' value (i.e.\ 'balance' between front and rear)
  -- * for the specified volume with the specified channel map. v will be
  -- * modified in place and returned. The balance is a value between
  -- * -1.0f and +1.0f. This operation might not be reversible! Also,
  -- * after this call pa_cvolume_get_fade() is not guaranteed to actually
  -- * return the requested fade value (e.g. when the input volume was
  -- * zero anyway for all channels). If no fade value is applicable to
  -- * this channel map the volume will not be modified. See
  -- * pa_channel_map_can_fade(). Will return NULL on error. \since 0.9.15  

   function pa_cvolume_set_fade
     (v : access pa_cvolume;
      map : access constant pulse_channelmap_h.pa_channel_map;
      new_fade : float) return access pa_cvolume  -- /usr/include/pulse/volume.h:374
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_set_fade";

  --* Calculate a 'lfe balance' value for the specified volume with
  -- * the specified channel map. The return value will range from
  -- * -1.0f (no lfe) to +1.0f (only lfe), where 0.0f is balanced.
  -- * If no value is applicable to this channel map the return value
  -- * will always be 0.0f. See pa_channel_map_can_lfe_balance(). \since 8.0  

   function pa_cvolume_get_lfe_balance (v : access constant pa_cvolume; map : access constant pulse_channelmap_h.pa_channel_map) return float  -- /usr/include/pulse/volume.h:381
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_get_lfe_balance";

  --* Adjust the 'lfe balance' value for the specified volume with
  -- * the specified channel map. v will be modified in place and returned.
  -- * The balance is a value between -1.0f (no lfe) and +1.0f (only lfe).
  -- * This operation might not be reversible! Also, after this call
  -- * pa_cvolume_get_lfe_balance() is not guaranteed to actually
  -- * return the requested value (e.g. when the input volume was
  -- * zero anyway for all channels). If no lfe balance value is applicable to
  -- * this channel map the volume will not be modified. See
  -- * pa_channel_map_can_lfe_balance(). Will return NULL on error. \since 8.0  

   function pa_cvolume_set_lfe_balance
     (v : access pa_cvolume;
      map : access constant pulse_channelmap_h.pa_channel_map;
      new_balance : float) return access pa_cvolume  -- /usr/include/pulse/volume.h:392
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_set_lfe_balance";

  --* Scale the passed pa_cvolume structure so that the maximum volume
  -- * of all channels equals max. The proportions between the channel
  -- * volumes are kept. Returns \a v, or NULL on error. \since 0.9.15  

   function pa_cvolume_scale (v : access pa_cvolume; max : pa_volume_t) return access pa_cvolume  -- /usr/include/pulse/volume.h:397
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_scale";

  --* Scale the passed pa_cvolume structure so that the maximum volume
  -- * of all channels selected via cm/mask equals max. This also modifies
  -- * the volume of those channels that are unmasked. The proportions
  -- * between the channel volumes are kept. If cm is NULL this call is
  -- * identical to pa_cvolume_scale(). Returns \a v, or NULL on error.
  -- * \since 0.9.16  

   function pa_cvolume_scale_mask
     (v : access pa_cvolume;
      max : pa_volume_t;
      cm : access constant pulse_channelmap_h.pa_channel_map;
      mask : pulse_channelmap_h.pa_channel_position_mask_t) return access pa_cvolume  -- /usr/include/pulse/volume.h:405
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_scale_mask";

  --* Set the passed volume to all channels at the specified channel
  -- * position. Will return the updated volume struct, or NULL if there
  -- * is no channel at the position specified. You can check if a channel
  -- * map includes a specific position by calling
  -- * pa_channel_map_has_position(). Returns \a cv, or NULL on error.
  -- * \since 0.9.16  

   function pa_cvolume_set_position
     (cv : access pa_cvolume;
      map : access constant pulse_channelmap_h.pa_channel_map;
      t : pulse_channelmap_h.pa_channel_position_t;
      v : pa_volume_t) return access pa_cvolume  -- /usr/include/pulse/volume.h:413
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_set_position";

  --* Get the maximum volume of all channels at the specified channel
  -- * position. Will return 0 if there is no channel at the position
  -- * specified. You can check if a channel map includes a specific
  -- * position by calling pa_channel_map_has_position(). \since 0.9.16  

   function pa_cvolume_get_position
     (cv : access constant pa_cvolume;
      map : access constant pulse_channelmap_h.pa_channel_map;
      t : pulse_channelmap_h.pa_channel_position_t) return pa_volume_t  -- /usr/include/pulse/volume.h:419
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_get_position";

  --* This goes through all channels in a and b and sets the
  -- * corresponding channel in dest to the greater volume of both. a, b
  -- * and dest may point to the same structure. Returns \a dest, or NULL
  -- * on error. \since 0.9.16  

   function pa_cvolume_merge
     (dest : access pa_cvolume;
      a : access constant pa_cvolume;
      b : access constant pa_cvolume) return access pa_cvolume  -- /usr/include/pulse/volume.h:425
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_merge";

  --* Increase the volume passed in by 'inc', but not exceeding 'limit'.
  -- * The proportions between the channels are kept.
  -- * Returns \a v, or NULL on error. \since 0.9.19  

   function pa_cvolume_inc_clamp
     (v : access pa_cvolume;
      inc : pa_volume_t;
      limit : pa_volume_t) return access pa_cvolume  -- /usr/include/pulse/volume.h:430
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_inc_clamp";

  --* Increase the volume passed in by 'inc'. The proportions between
  -- * the channels are kept. Returns \a v, or NULL on error. \since 0.9.16  

   function pa_cvolume_inc (v : access pa_cvolume; inc : pa_volume_t) return access pa_cvolume  -- /usr/include/pulse/volume.h:434
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_inc";

  --* Decrease the volume passed in by 'dec'. The proportions between
  -- * the channels are kept. Returns \a v, or NULL on error. \since 0.9.16  

   function pa_cvolume_dec (v : access pa_cvolume; dec : pa_volume_t) return access pa_cvolume  -- /usr/include/pulse/volume.h:438
   with Import => True, 
        Convention => C, 
        External_Name => "pa_cvolume_dec";

end pulse_volume_h;
