pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with bits_stdint_uintn_h;
with pulse_sample_h;
with pulse_channelmap_h;
with pulse_volume_h;
with pulse_def_h;
limited with pulse_proplist_h;
with System;
with pulse_context_h;
limited with pulse_operation_h;
with bits_stdint_intn_h;
limited with pulse_format_h;

package pulse_introspect_h is

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

  --* \page introspect Server Query and Control
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * Sometimes it is necessary to query and modify global settings in the
  -- * server. For this, PulseAudio has the introspection API. It can list sinks,
  -- * sources, samples and other aspects of the server. It can also modify the
  -- * attributes of the server that will affect operations on a global level,
  -- * and not just the application's context.
  -- *
  -- * \section query_sec Querying
  -- *
  -- * All querying is done through callbacks. This approach is necessary to
  -- * maintain an asynchronous design. The client will request the information
  -- * and some time later, the server will respond with the desired data.
  -- *
  -- * Some objects can have multiple instances on the server. When requesting all
  -- * of these at once, the callback will be called multiple times, once for
  -- * each object. When the list has been exhausted, the callback will be called
  -- * without an information structure and the eol parameter set to a positive
  -- * value.
  -- *
  -- * Note that even if a single object is requested, and not the entire list,
  -- * the terminating call will still be made.
  -- *
  -- * If an error occurs, the callback will be invoked without an information
  -- * structure and eol set to a negative value..
  -- *
  -- * Data members in the information structures are only valid during the
  -- * duration of the callback. If they are required after the callback is
  -- * finished, a deep copy of the information structure must be performed.
  -- *
  -- * \subsection server_subsec Server Information
  -- *
  -- * The server can be queried about its name, the environment it's running on
  -- * and the currently active global defaults. Calling
  -- * pa_context_get_server_info() provides access to a pa_server_info structure
  -- * containing all of these.
  -- *
  -- * \subsection memstat_subsec Memory Usage
  -- *
  -- * Statistics about memory usage can be fetched using pa_context_stat(),
  -- * giving a pa_stat_info structure.
  -- *
  -- * \subsection sinksrc_subsec Sinks and Sources
  -- *
  -- * The server can have an arbitrary number of sinks and sources. Each sink
  -- * and source have both an index and a name associated with it. As such,
  -- * there are three ways to get access to them:
  -- *
  -- * \li By index - pa_context_get_sink_info_by_index() /
  -- *                pa_context_get_source_info_by_index()
  -- * \li By name - pa_context_get_sink_info_by_name() /
  -- *               pa_context_get_source_info_by_name()
  -- * \li All - pa_context_get_sink_info_list() /
  -- *           pa_context_get_source_info_list()
  -- *
  -- * All three method use the same callback and will provide a pa_sink_info or
  -- * pa_source_info structure.
  -- *
  -- * \subsection siso_subsec Sink Inputs and Source Outputs
  -- *
  -- * Sink inputs and source outputs are the representations of the client ends
  -- * of streams inside the server. I.e. they connect a client stream to one of
  -- * the global sinks or sources.
  -- *
  -- * Sink inputs and source outputs only have an index to identify them. As
  -- * such, there are only two ways to get information about them:
  -- *
  -- * \li By index - pa_context_get_sink_input_info() /
  -- *                pa_context_get_source_output_info()
  -- * \li All - pa_context_get_sink_input_info_list() /
  -- *           pa_context_get_source_output_info_list()
  -- *
  -- * The structure returned is the pa_sink_input_info or pa_source_output_info
  -- * structure.
  -- *
  -- * \subsection samples_subsec Samples
  -- *
  -- * The list of cached samples can be retrieved from the server. Three methods
  -- * exist for querying the sample cache list:
  -- *
  -- * \li By index - pa_context_get_sample_info_by_index()
  -- * \li By name - pa_context_get_sample_info_by_name()
  -- * \li All - pa_context_get_sample_info_list()
  -- *
  -- * Note that this only retrieves information about the sample, not the sample
  -- * data itself.
  -- *
  -- * \subsection module_subsec Driver Modules
  -- *
  -- * PulseAudio driver modules are identified by index and are retrieved using either
  -- * pa_context_get_module_info() or pa_context_get_module_info_list(). The
  -- * information structure is called pa_module_info.
  -- *
  -- * \subsection client_subsec Clients
  -- *
  -- * PulseAudio clients are also identified by index and are retrieved using
  -- * either pa_context_get_client_info() or pa_context_get_client_info_list().
  -- * The information structure is called pa_client_info.
  -- *
  -- * \section ctrl_sec Control
  -- *
  -- * Some parts of the server are only possible to read, but most can also be
  -- * modified in different ways. Note that these changes will affect all
  -- * connected clients and not just the one issuing the request.
  -- *
  -- * \subsection sinksrc_subsec Sinks and Sources
  -- *
  -- * The most common change one would want to apply to sinks and sources is to
  -- * modify the volume of the audio. Identically to how sinks and sources can
  -- * be queried, there are two ways of identifying them:
  -- *
  -- * \li By index - pa_context_set_sink_volume_by_index() /
  -- *                pa_context_set_source_volume_by_index()
  -- * \li By name - pa_context_set_sink_volume_by_name() /
  -- *               pa_context_set_source_volume_by_name()
  -- *
  -- * It is also possible to mute a sink or source:
  -- *
  -- * \li By index - pa_context_set_sink_mute_by_index() /
  -- *                pa_context_set_source_mute_by_index()
  -- * \li By name - pa_context_set_sink_mute_by_name() /
  -- *               pa_context_set_source_mute_by_name()
  -- *
  -- * \subsection siso_subsec Sink Inputs and Source Outputs
  -- *
  -- * If an application desires to modify the volume of just a single stream
  -- * (commonly one of its own streams), this can be done by setting the volume
  -- * of its associated sink input or source output, using
  -- * pa_context_set_sink_input_volume() or pa_context_set_source_output_volume().
  -- *
  -- * It is also possible to remove sink inputs and source outputs, terminating
  -- * the streams associated with them:
  -- *
  -- * \li Sink input - pa_context_kill_sink_input()
  -- * \li Source output - pa_context_kill_source_output()
  -- *
  -- * It is strongly recommended that all volume changes are done as a direct
  -- * result of user input. With automated requests, such as those resulting
  -- * from misguided attempts of crossfading, PulseAudio can store the stream
  -- * volume at an inappropriate moment and restore it later. Besides, such
  -- * attempts lead to OSD popups in some desktop environments.
  -- *
  -- * As a special case of the general rule above, it is recommended that your
  -- * application leaves the task of saving and restoring the volume of its
  -- * streams to PulseAudio and does not attempt to do it by itself. PulseAudio
  -- * really knows better about events such as stream moving or headphone
  -- * plugging that would make the volume stored by the application inapplicable
  -- * to the new configuration.
  -- *
  -- * Another important case where setting a sink input volume may be a bad idea
  -- * is related to interpreters that interpret potentially untrusted scripts.
  -- * PulseAudio relies on your application not making malicious requests (such
  -- * as repeatedly setting the volume to 100%). Thus, script interpreters that
  -- * represent a security boundary must sandbox volume-changing requests coming
  -- * from their scripts. In the worst case, it may be necessary to apply the
  -- * script-requested volume to the script-produced sounds by altering the
  -- * samples in the script interpreter and not touching the sink or sink input
  -- * volume as seen by PulseAudio.
  -- *
  -- * If an application changes any volume, it should also listen to changes of
  -- * the same volume originating from outside the application (e.g., from the
  -- * system mixer application) and update its user interface accordingly. Use
  -- * \ref subscribe to get such notifications.
  -- *
  -- * \subsection module_subsec Modules
  -- *
  -- * Server modules can be remotely loaded and unloaded using
  -- * pa_context_load_module() and pa_context_unload_module().
  -- *
  -- * \subsection message_subsec Messages
  -- *
  -- * Server objects like sinks, sink inputs or modules can register a message
  -- * handler to communicate with clients. A message can be sent to a named
  -- * message handler using pa_context_send_message_to_object().
  -- *
  -- * \subsection client_subsec Clients
  -- *
  -- * The only operation supported on clients is the possibility of kicking
  -- * them off the server using pa_context_kill_client().
  --  

  --* \file
  -- *
  -- * Routines for daemon introspection.
  -- *
  -- * See also \subpage introspect
  --  

  --* @{ \name Sinks  
  --* Stores information about a specific port of a sink.  Please
  -- * note that this structure can be extended as part of evolutionary
  -- * API updates at any time in any new release. \since 0.9.16  

  --*< Name of this port  
   type pa_sink_port_info is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:234
      description : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:235
      priority : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:236
      available : aliased int;  -- /usr/include/pulse/introspect.h:237
      availability_group : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:238
      c_type : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:261
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:233

  --*< Description of this port  
  --*< The higher this value is, the more useful this port is as a default.  
  --*< A flags (see #pa_port_available), indicating availability status of this port. \since 2.0  
  --*< An indentifier for the group of ports that share their availability status with
  --                                         * each other. This is meant especially for handling cases where one 3.5 mm connector
  --                                         * is used for headphones, headsets and microphones, and the hardware can only tell
  --                                         * that something was plugged in but not what exactly. In this situation the ports for
  --                                         * all those devices share their availability status, and PulseAudio can't tell which
  --                                         * one is actually plugged in, and some application may ask the user what was plugged
  --                                         * in. Such applications should get a list of all card ports and compare their
  --                                         * `availability_group` fields. Ports that have the same group are those that need
  --                                         * input from the user to determine which device was plugged in. The application should
  --                                         * then activate the user-chosen port.
  --                                         *
  --                                         * May be NULL, in which case the port is not part of any availability group.
  --                                         *
  --                                         * The group identifier must be treated as an opaque identifier. The string may look
  --                                         * like an ALSA control name, but applications must not assume any such relationship.
  --                                         * The group naming scheme can change without a warning.
  --                                         *
  --                                         * Since one group can include both input and output ports, the grouping should be done
  --                                         * using pa_card_port_info instead of pa_sink_port_info, but this field is duplicated
  --                                         * also in pa_sink_port_info (and pa_source_port_info) in case someone finds that
  --                                         * convenient.
  --                                         *
  --                                         * \since 14.0  

  --*< Port type, see #pa_device_port_type. \since 14.0  
  --* Stores information about sinks. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  

  --*< Name of the sink  
   type pa_sink_info is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:268
      index : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:269
      description : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:270
      sample_spec : aliased pulse_sample_h.pa_sample_spec;  -- /usr/include/pulse/introspect.h:271
      channel_map : aliased pulse_channelmap_h.pa_channel_map;  -- /usr/include/pulse/introspect.h:272
      owner_module : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:273
      volume : aliased pulse_volume_h.pa_cvolume;  -- /usr/include/pulse/introspect.h:274
      mute : aliased int;  -- /usr/include/pulse/introspect.h:275
      monitor_source : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:276
      monitor_source_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:277
      latency : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/introspect.h:278
      driver : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:279
      flags : aliased pulse_def_h.pa_sink_flags_t;  -- /usr/include/pulse/introspect.h:280
      proplist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/introspect.h:281
      configured_latency : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/introspect.h:282
      base_volume : aliased pulse_volume_h.pa_volume_t;  -- /usr/include/pulse/introspect.h:283
      state : aliased pulse_def_h.pa_sink_state_t;  -- /usr/include/pulse/introspect.h:284
      n_volume_steps : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:285
      card : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:286
      n_ports : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:287
      ports : System.Address;  -- /usr/include/pulse/introspect.h:288
      active_port : access pa_sink_port_info;  -- /usr/include/pulse/introspect.h:289
      n_formats : aliased bits_stdint_uintn_h.uint8_t;  -- /usr/include/pulse/introspect.h:290
      formats : System.Address;  -- /usr/include/pulse/introspect.h:291
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:267

  --*< Index of the sink  
  --*< Description of this sink  
  --*< Sample spec of this sink  
  --*< Channel map  
  --*< Index of the owning module of this sink, or PA_INVALID_INDEX.  
  --*< Volume of the sink  
  --*< Mute switch of the sink  
  --*< Index of the monitor source connected to this sink.  
  --*< The name of the monitor source.  
  --*< Length of queued audio in the output buffer.  
  --*< Driver name  
  --*< Flags  
  --*< Property list \since 0.9.11  
  --*< The latency this device has been configured to. \since 0.9.11  
  --*< Some kind of "base" volume that refers to unamplified/unattenuated volume in the context of the output device. \since 0.9.15  
  --*< State \since 0.9.15  
  --*< Number of volume steps for sinks which do not support arbitrary volumes. \since 0.9.15  
  --*< Card index, or PA_INVALID_INDEX. \since 0.9.15  
  --*< Number of entries in port array \since 0.9.16  
  --*< Array of available ports, or NULL. Array is terminated by an entry set to NULL. The number of entries is stored in n_ports. \since 0.9.16  
  --*< Pointer to active port in the array, or NULL. \since 0.9.16  
  --*< Number of formats supported by the sink. \since 1.0  
  --*< Array of formats supported by the sink. \since 1.0  
  --* Callback prototype for pa_context_get_sink_info_by_name() and friends  
   type pa_sink_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_sink_info;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:295

  --* Get information about a sink by its name  
   function pa_context_get_sink_info_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_sink_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:298
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_sink_info_by_name";

  --* Get information about a sink by its index  
   function pa_context_get_sink_info_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pa_sink_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:301
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_sink_info_by_index";

  --* Get the complete sink list  
   function pa_context_get_sink_info_list
     (c : access pulse_context_h.pa_context;
      cb : pa_sink_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:304
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_sink_info_list";

  --* Set the volume of a sink device specified by its index  
   function pa_context_set_sink_volume_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      volume : access constant pulse_volume_h.pa_cvolume;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:307
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_sink_volume_by_index";

  --* Set the volume of a sink device specified by its name  
   function pa_context_set_sink_volume_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      volume : access constant pulse_volume_h.pa_cvolume;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_sink_volume_by_name";

  --* Set the mute switch of a sink device specified by its index  
   function pa_context_set_sink_mute_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      mute : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:313
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_sink_mute_by_index";

  --* Set the mute switch of a sink device specified by its name  
   function pa_context_set_sink_mute_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      mute : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:316
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_sink_mute_by_name";

  --* Suspend/Resume a sink. \since 0.9.7  
   function pa_context_suspend_sink_by_name
     (c : access pulse_context_h.pa_context;
      sink_name : Interfaces.C.Strings.chars_ptr;
      suspend : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:319
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_suspend_sink_by_name";

  --* Suspend/Resume a sink. If idx is PA_INVALID_INDEX all sinks will be suspended. \since 0.9.7  
   function pa_context_suspend_sink_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      suspend : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:322
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_suspend_sink_by_index";

  --* Change the profile of a sink. \since 0.9.16  
   function pa_context_set_sink_port_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      port : Interfaces.C.Strings.chars_ptr;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:325
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_sink_port_by_index";

  --* Change the profile of a sink. \since 0.9.15  
   function pa_context_set_sink_port_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      port : Interfaces.C.Strings.chars_ptr;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:328
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_sink_port_by_name";

  --* @}  
  --* @{ \name Sources  
  --* Stores information about a specific port of a source.  Please
  -- * note that this structure can be extended as part of evolutionary
  -- * API updates at any time in any new release. \since 0.9.16  

  --*< Name of this port  
   type pa_source_port_info is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:338
      description : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:339
      priority : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:340
      available : aliased int;  -- /usr/include/pulse/introspect.h:341
      availability_group : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:342
      c_type : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:366
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:337

  --*< Description of this port  
  --*< The higher this value is, the more useful this port is as a default.  
  --*< A flags (see #pa_port_available), indicating availability status of this port. \since 2.0  
  --*< An indentifier for the group of ports that share their availability status with
  --                                         * each other. This is meant especially for handling cases where one 3.5 mm connector
  --                                         * is used for headphones, headsets and microphones, and the hardware can only tell
  --                                         * that something was plugged in but not what exactly. In this situation the ports for
  --                                         * all those devices share their availability status, and PulseAudio can't tell which
  --                                         * one is actually plugged in, and some application may ask the user what was plugged
  --                                         * in. Such applications should get a list of all card ports and compare their
  --                                         * `availability_group` fields. Ports that have the same group are those that need
  --                                         * input from the user to determine which device was plugged in. The application should
  --                                         * then activate the user-chosen port.
  --                                         *
  --                                         * May be NULL, in which case the port is not part of any availability group (which is
  --                                         * the same as having a group with only one member).
  --                                         *
  --                                         * The group identifier must be treated as an opaque identifier. The string may look
  --                                         * like an ALSA control name, but applications must not assume any such relationship.
  --                                         * The group naming scheme can change without a warning.
  --                                         *
  --                                         * Since one group can include both input and output ports, the grouping should be done
  --                                         * using pa_card_port_info instead of pa_source_port_info, but this field is duplicated
  --                                         * also in pa_source_port_info (and pa_sink_port_info) in case someone finds that
  --                                         * convenient.
  --                                         *
  --                                         * \since 14.0  

  --*< Port type, see #pa_device_port_type. \since 14.0  
  --* Stores information about sources. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  

  --*< Name of the source  
   type pa_source_info is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:373
      index : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:374
      description : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:375
      sample_spec : aliased pulse_sample_h.pa_sample_spec;  -- /usr/include/pulse/introspect.h:376
      channel_map : aliased pulse_channelmap_h.pa_channel_map;  -- /usr/include/pulse/introspect.h:377
      owner_module : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:378
      volume : aliased pulse_volume_h.pa_cvolume;  -- /usr/include/pulse/introspect.h:379
      mute : aliased int;  -- /usr/include/pulse/introspect.h:380
      monitor_of_sink : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:381
      monitor_of_sink_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:382
      latency : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/introspect.h:383
      driver : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:384
      flags : aliased pulse_def_h.pa_source_flags_t;  -- /usr/include/pulse/introspect.h:385
      proplist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/introspect.h:386
      configured_latency : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/introspect.h:387
      base_volume : aliased pulse_volume_h.pa_volume_t;  -- /usr/include/pulse/introspect.h:388
      state : aliased pulse_def_h.pa_source_state_t;  -- /usr/include/pulse/introspect.h:389
      n_volume_steps : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:390
      card : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:391
      n_ports : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:392
      ports : System.Address;  -- /usr/include/pulse/introspect.h:393
      active_port : access pa_source_port_info;  -- /usr/include/pulse/introspect.h:394
      n_formats : aliased bits_stdint_uintn_h.uint8_t;  -- /usr/include/pulse/introspect.h:395
      formats : System.Address;  -- /usr/include/pulse/introspect.h:396
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:372

  --*< Index of the source  
  --*< Description of this source  
  --*< Sample spec of this source  
  --*< Channel map  
  --*< Owning module index, or PA_INVALID_INDEX.  
  --*< Volume of the source  
  --*< Mute switch of the sink  
  --*< If this is a monitor source, the index of the owning sink, otherwise PA_INVALID_INDEX.  
  --*< Name of the owning sink, or NULL.  
  --*< Length of filled record buffer of this source.  
  --*< Driver name  
  --*< Flags  
  --*< Property list \since 0.9.11  
  --*< The latency this device has been configured to. \since 0.9.11  
  --*< Some kind of "base" volume that refers to unamplified/unattenuated volume in the context of the input device. \since 0.9.15  
  --*< State \since 0.9.15  
  --*< Number of volume steps for sources which do not support arbitrary volumes. \since 0.9.15  
  --*< Card index, or PA_INVALID_INDEX. \since 0.9.15  
  --*< Number of entries in port array \since 0.9.16  
  --*< Array of available ports, or NULL. Array is terminated by an entry set to NULL. The number of entries is stored in n_ports. \since 0.9.16   
  --*< Pointer to active port in the array, or NULL. \since 0.9.16   
  --*< Number of formats supported by the source. \since 1.0  
  --*< Array of formats supported by the source. \since 1.0  
  --* Callback prototype for pa_context_get_source_info_by_name() and friends  
   type pa_source_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_source_info;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:400

  --* Get information about a source by its name  
   function pa_context_get_source_info_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_source_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:403
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_source_info_by_name";

  --* Get information about a source by its index  
   function pa_context_get_source_info_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pa_source_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:406
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_source_info_by_index";

  --* Get the complete source list  
   function pa_context_get_source_info_list
     (c : access pulse_context_h.pa_context;
      cb : pa_source_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:409
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_source_info_list";

  --* Set the volume of a source device specified by its index  
   function pa_context_set_source_volume_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      volume : access constant pulse_volume_h.pa_cvolume;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:412
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_source_volume_by_index";

  --* Set the volume of a source device specified by its name  
   function pa_context_set_source_volume_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      volume : access constant pulse_volume_h.pa_cvolume;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:415
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_source_volume_by_name";

  --* Set the mute switch of a source device specified by its index  
   function pa_context_set_source_mute_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      mute : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:418
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_source_mute_by_index";

  --* Set the mute switch of a source device specified by its name  
   function pa_context_set_source_mute_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      mute : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:421
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_source_mute_by_name";

  --* Suspend/Resume a source. \since 0.9.7  
   function pa_context_suspend_source_by_name
     (c : access pulse_context_h.pa_context;
      source_name : Interfaces.C.Strings.chars_ptr;
      suspend : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:424
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_suspend_source_by_name";

  --* Suspend/Resume a source. If idx is PA_INVALID_INDEX, all sources will be suspended. \since 0.9.7  
   function pa_context_suspend_source_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      suspend : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:427
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_suspend_source_by_index";

  --* Change the profile of a source. \since 0.9.16  
   function pa_context_set_source_port_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      port : Interfaces.C.Strings.chars_ptr;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:430
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_source_port_by_index";

  --* Change the profile of a source. \since 0.9.15  
   function pa_context_set_source_port_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      port : Interfaces.C.Strings.chars_ptr;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:433
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_source_port_by_name";

  --* @}  
  --* @{ \name Server  
  --* Server information. Please note that this structure can be
  -- * extended as part of evolutionary API updates at any time in any new
  -- * release.  

  --*< User name of the daemon process  
   type pa_server_info is record
      user_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:443
      host_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:444
      server_version : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:445
      server_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:446
      sample_spec : aliased pulse_sample_h.pa_sample_spec;  -- /usr/include/pulse/introspect.h:447
      default_sink_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:448
      default_source_name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:449
      cookie : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:450
      channel_map : aliased pulse_channelmap_h.pa_channel_map;  -- /usr/include/pulse/introspect.h:451
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:442

  --*< Host name the daemon is running on  
  --*< Version string of the daemon  
  --*< Server package name (usually "pulseaudio")  
  --*< Default sample specification  
  --*< Name of default sink.  
  --*< Name of default source.  
  --*< A random cookie for identifying this instance of PulseAudio.  
  --*< Default channel map. \since 0.9.15  
  --* Callback prototype for pa_context_get_server_info()  
   type pa_server_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_server_info;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:455

  --* Get some information about the server  
   function pa_context_get_server_info
     (c : access pulse_context_h.pa_context;
      cb : pa_server_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:458
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_server_info";

  --* @}  
  --* @{ \name Modules  
  --* Stores information about modules. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  

  --*< Index of the module  
   type pa_module_info is record
      index : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:468
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:469
      argument : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:470
      n_used : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:471
      auto_unload : aliased int;  -- /usr/include/pulse/introspect.h:473
      proplist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/introspect.h:475
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:467

  --*< Name of the module  
  --*< Argument string of the module  
  --*< Usage counter or PA_INVALID_INDEX  
  --* \cond fulldocs  
  --*< \deprecated Non-zero if this is an autoloaded module.  
  --* \endcond  
  --*< Property list \since 0.9.15  
  --* Callback prototype for pa_context_get_module_info() and friends  
   type pa_module_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_module_info;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:479

  --* Get some information about a module by its index  
   function pa_context_get_module_info
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pa_module_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:482
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_module_info";

  --* Get the complete list of currently loaded modules  
   function pa_context_get_module_info_list
     (c : access pulse_context_h.pa_context;
      cb : pa_module_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:485
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_module_info_list";

  --* Callback prototype for pa_context_load_module()  
   type pa_context_index_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : bits_stdint_uintn_h.uint32_t;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:488

  --* Load a module.  
   function pa_context_load_module
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      argument : Interfaces.C.Strings.chars_ptr;
      cb : pa_context_index_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:491
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_load_module";

  --* Unload a module.  
   function pa_context_unload_module
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:494
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_unload_module";

  --* @}  
  --* @{ \name Messages  
  --* Callback prototype for pa_context_send_message_to_object() \since 15.0  
   type pa_context_string_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : int;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:501

  --* Send a message to an object that registered a message handler. For more information
  -- * see https://cgit.freedesktop.org/pulseaudio/pulseaudio/tree/doc/messaging_api.txt. \since 15.0  

   function pa_context_send_message_to_object
     (c : access pulse_context_h.pa_context;
      recipient_name : Interfaces.C.Strings.chars_ptr;
      message : Interfaces.C.Strings.chars_ptr;
      message_parameters : Interfaces.C.Strings.chars_ptr;
      cb : pa_context_string_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:505
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_send_message_to_object";

  --* @}  
  --* @{ \name Clients  
  --* Stores information about clients. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  

  --*< Index of this client  
   type pa_client_info is record
      index : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:515
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:516
      owner_module : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:517
      driver : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:518
      proplist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/introspect.h:519
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:514

  --*< Name of this client  
  --*< Index of the owning module, or PA_INVALID_INDEX.  
  --*< Driver name  
  --*< Property list \since 0.9.11  
  --* Callback prototype for pa_context_get_client_info() and friends  
   type pa_client_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_client_info;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:523

  --* Get information about a client by its index  
   function pa_context_get_client_info
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pa_client_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:526
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_client_info";

  --* Get the complete client list  
   function pa_context_get_client_info_list
     (c : access pulse_context_h.pa_context;
      cb : pa_client_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:529
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_client_info_list";

  --* Kill a client.  
   function pa_context_kill_client
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:532
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_kill_client";

  --* @}  
  --* @{ \name Cards  
  --* \deprecated Superseded by pa_card_profile_info2 \since 0.9.15  
  --*< Name of this profile  
   type pa_card_profile_info is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:540
      description : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:541
      n_sinks : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:542
      n_sources : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:543
      priority : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:544
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:539

  --*< Description of this profile  
  --*< Number of sinks this profile would create  
  --*< Number of sources this profile would create  
  --*< The higher this value is, the more useful this profile is as a default.  
  --* Stores information about a specific profile of a card. Please
  -- * note that this structure can be extended as part of evolutionary
  -- * API updates at any time in any new release. \since 5.0  

  --*< Name of this profile  
   type pa_card_profile_info2 is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:551
      description : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:552
      n_sinks : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:553
      n_sources : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:554
      priority : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:555
      available : aliased int;  -- /usr/include/pulse/introspect.h:556
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:550

  --*< Description of this profile  
  --*< Number of sinks this profile would create  
  --*< Number of sources this profile would create  
  --*< The higher this value is, the more useful this profile is as a default.  
  --*< Is this profile available? If this is zero, meaning "unavailable",
  --     * then it makes no sense to try to activate this profile. If this is
  --     * non-zero, it's still not a guarantee that activating the profile will
  --     * result in anything useful, it just means that the server isn't aware of
  --     * any reason why the profile would definitely be useless. \since 5.0  

  --* Stores information about a specific port of a card.  Please
  -- * note that this structure can be extended as part of evolutionary
  -- * API updates at any time in any new release. \since 2.0  

  --*< Name of this port  
   type pa_card_port_info is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:568
      description : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:569
      priority : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:570
      available : aliased int;  -- /usr/include/pulse/introspect.h:571
      direction : aliased int;  -- /usr/include/pulse/introspect.h:572
      n_profiles : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:573
      profiles : System.Address;  -- /usr/include/pulse/introspect.h:574
      proplist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/introspect.h:575
      latency_offset : aliased bits_stdint_intn_h.int64_t;  -- /usr/include/pulse/introspect.h:576
      profiles2 : System.Address;  -- /usr/include/pulse/introspect.h:577
      availability_group : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:578
      c_type : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:597
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:567

  --*< Description of this port  
  --*< The higher this value is, the more useful this port is as a default.  
  --*< A #pa_port_available enum, indicating availability status of this port.  
  --*< A #pa_direction enum, indicating the direction of this port.  
  --*< Number of entries in profile array  
  --*< \deprecated Superseded by profiles2  
  --*< Property list  
  --*< Latency offset of the port that gets added to the sink/source latency when the port is active. \since 3.0  
  --*< Array of pointers to available profiles, or NULL. Array is terminated by an entry set to NULL. \since 5.0  
  --*< An indentifier for the group of ports that share their availability status with
  --                                         * each other. This is meant especially for handling cases where one 3.5 mm connector
  --                                         * is used for headphones, headsets and microphones, and the hardware can only tell
  --                                         * that something was plugged in but not what exactly. In this situation the ports for
  --                                         * all those devices share their availability status, and PulseAudio can't tell which
  --                                         * one is actually plugged in, and some application may ask the user what was plugged
  --                                         * in. Such applications should get a list of all card ports and compare their
  --                                         * `availability_group` fields. Ports that have the same group are those that need
  --                                         * input from the user to determine which device was plugged in. The application should
  --                                         * then activate the user-chosen port.
  --                                         *
  --                                         * May be NULL, in which case the port is not part of any availability group (which is
  --                                         * the same as having a group with only one member).
  --                                         *
  --                                         * The group identifier must be treated as an opaque identifier. The string may look
  --                                         * like an ALSA control name, but applications must not assume any such relationship.
  --                                         * The group naming scheme can change without a warning.
  --                                         *
  --                                         * \since 14.0  

  --*< Port type, see #pa_device_port_type. \since 14.0  
  --* Stores information about cards. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  \since 0.9.15  

  --*< Index of this card  
   type pa_card_info is record
      index : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:604
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:605
      owner_module : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:606
      driver : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:607
      n_profiles : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:608
      profiles : access pa_card_profile_info;  -- /usr/include/pulse/introspect.h:609
      active_profile : access pa_card_profile_info;  -- /usr/include/pulse/introspect.h:610
      proplist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/introspect.h:611
      n_ports : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:612
      ports : System.Address;  -- /usr/include/pulse/introspect.h:613
      profiles2 : System.Address;  -- /usr/include/pulse/introspect.h:614
      active_profile2 : access pa_card_profile_info2;  -- /usr/include/pulse/introspect.h:615
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:603

  --*< Name of this card  
  --*< Index of the owning module, or PA_INVALID_INDEX.  
  --*< Driver name  
  --*< Number of entries in profile array  
  --*< \deprecated Superseded by profiles2  
  --*< \deprecated Superseded by active_profile2  
  --*< Property list  
  --*< Number of entries in port array  
  --*< Array of pointers to ports, or NULL. Array is terminated by an entry set to NULL.  
  --*< Array of pointers to available profiles, or NULL. Array is terminated by an entry set to NULL. \since 5.0  
  --*< Pointer to active profile in the array, or NULL. \since 5.0  
  --* Callback prototype for pa_context_get_card_info_...() \since 0.9.15  
   type pa_card_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_card_info;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:619

  --* Get information about a card by its index \since 0.9.15  
   function pa_context_get_card_info_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pa_card_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:622
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_card_info_by_index";

  --* Get information about a card by its name \since 0.9.15  
   function pa_context_get_card_info_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_card_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:625
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_card_info_by_name";

  --* Get the complete card list \since 0.9.15  
   function pa_context_get_card_info_list
     (c : access pulse_context_h.pa_context;
      cb : pa_card_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:628
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_card_info_list";

  --* Change the profile of a card. \since 0.9.15  
   function pa_context_set_card_profile_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      profile : Interfaces.C.Strings.chars_ptr;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:631
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_card_profile_by_index";

  --* Change the profile of a card. \since 0.9.15  
   function pa_context_set_card_profile_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      profile : Interfaces.C.Strings.chars_ptr;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:634
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_card_profile_by_name";

  --* Set the latency offset of a port. \since 3.0  
   function pa_context_set_port_latency_offset
     (c : access pulse_context_h.pa_context;
      card_name : Interfaces.C.Strings.chars_ptr;
      port_name : Interfaces.C.Strings.chars_ptr;
      offset : bits_stdint_intn_h.int64_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:637
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_port_latency_offset";

  --* @}  
  --* @{ \name Sink Inputs  
  --* Stores information about sink inputs. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  

  --*< Index of the sink input  
   type pa_sink_input_info is record
      index : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:647
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:648
      owner_module : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:649
      client : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:650
      sink : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:651
      sample_spec : aliased pulse_sample_h.pa_sample_spec;  -- /usr/include/pulse/introspect.h:652
      channel_map : aliased pulse_channelmap_h.pa_channel_map;  -- /usr/include/pulse/introspect.h:653
      volume : aliased pulse_volume_h.pa_cvolume;  -- /usr/include/pulse/introspect.h:654
      buffer_usec : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/introspect.h:655
      sink_usec : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/introspect.h:656
      resample_method : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:657
      driver : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:658
      mute : aliased int;  -- /usr/include/pulse/introspect.h:659
      proplist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/introspect.h:660
      corked : aliased int;  -- /usr/include/pulse/introspect.h:661
      has_volume : aliased int;  -- /usr/include/pulse/introspect.h:662
      volume_writable : aliased int;  -- /usr/include/pulse/introspect.h:663
      format : access pulse_format_h.pa_format_info;  -- /usr/include/pulse/introspect.h:664
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:646

  --*< Name of the sink input  
  --*< Index of the module this sink input belongs to, or PA_INVALID_INDEX when it does not belong to any module.  
  --*< Index of the client this sink input belongs to, or PA_INVALID_INDEX when it does not belong to any client.  
  --*< Index of the connected sink  
  --*< The sample specification of the sink input.  
  --*< Channel map  
  --*< The volume of this sink input.  
  --*< Latency due to buffering in sink input, see pa_timing_info for details.  
  --*< Latency of the sink device, see pa_timing_info for details.  
  --*< The resampling method used by this sink input.  
  --*< Driver name  
  --*< Stream muted \since 0.9.7  
  --*< Property list \since 0.9.11  
  --*< Stream corked \since 1.0  
  --*< Stream has volume. If not set, then the meaning of this struct's volume member is unspecified. \since 1.0  
  --*< The volume can be set. If not set, the volume can still change even though clients can't control the volume. \since 1.0  
  --*< Stream format information. \since 1.0  
  --* Callback prototype for pa_context_get_sink_input_info() and friends  
   type pa_sink_input_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_sink_input_info;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:668

  --* Get some information about a sink input by its index  
   function pa_context_get_sink_input_info
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pa_sink_input_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:671
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_sink_input_info";

  --* Get the complete sink input list  
   function pa_context_get_sink_input_info_list
     (c : access pulse_context_h.pa_context;
      cb : pa_sink_input_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:674
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_sink_input_info_list";

  --* Move the specified sink input to a different sink. \since 0.9.5  
   function pa_context_move_sink_input_by_name
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      sink_name : Interfaces.C.Strings.chars_ptr;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:677
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_move_sink_input_by_name";

  --* Move the specified sink input to a different sink. \since 0.9.5  
   function pa_context_move_sink_input_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      sink_idx : bits_stdint_uintn_h.uint32_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:680
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_move_sink_input_by_index";

  --* Set the volume of a sink input stream  
   function pa_context_set_sink_input_volume
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      volume : access constant pulse_volume_h.pa_cvolume;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:683
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_sink_input_volume";

  --* Set the mute switch of a sink input stream \since 0.9.7  
   function pa_context_set_sink_input_mute
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      mute : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:686
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_sink_input_mute";

  --* Kill a sink input.  
   function pa_context_kill_sink_input
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:689
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_kill_sink_input";

  --* @}  
  --* @{ \name Source Outputs  
  --* Stores information about source outputs. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  

  --*< Index of the source output  
   type pa_source_output_info is record
      index : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:699
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:700
      owner_module : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:701
      client : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:702
      source : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:703
      sample_spec : aliased pulse_sample_h.pa_sample_spec;  -- /usr/include/pulse/introspect.h:704
      channel_map : aliased pulse_channelmap_h.pa_channel_map;  -- /usr/include/pulse/introspect.h:705
      buffer_usec : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/introspect.h:706
      source_usec : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/introspect.h:707
      resample_method : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:708
      driver : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:709
      proplist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/introspect.h:710
      corked : aliased int;  -- /usr/include/pulse/introspect.h:711
      volume : aliased pulse_volume_h.pa_cvolume;  -- /usr/include/pulse/introspect.h:712
      mute : aliased int;  -- /usr/include/pulse/introspect.h:713
      has_volume : aliased int;  -- /usr/include/pulse/introspect.h:714
      volume_writable : aliased int;  -- /usr/include/pulse/introspect.h:715
      format : access pulse_format_h.pa_format_info;  -- /usr/include/pulse/introspect.h:716
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:698

  --*< Name of the source output  
  --*< Index of the module this source output belongs to, or PA_INVALID_INDEX when it does not belong to any module.  
  --*< Index of the client this source output belongs to, or PA_INVALID_INDEX when it does not belong to any client.  
  --*< Index of the connected source  
  --*< The sample specification of the source output  
  --*< Channel map  
  --*< Latency due to buffering in the source output, see pa_timing_info for details.  
  --*< Latency of the source device, see pa_timing_info for details.  
  --*< The resampling method used by this source output.  
  --*< Driver name  
  --*< Property list \since 0.9.11  
  --*< Stream corked \since 1.0  
  --*< The volume of this source output \since 1.0  
  --*< Stream muted \since 1.0  
  --*< Stream has volume. If not set, then the meaning of this struct's volume member is unspecified. \since 1.0  
  --*< The volume can be set. If not set, the volume can still change even though clients can't control the volume. \since 1.0  
  --*< Stream format information. \since 1.0  
  --* Callback prototype for pa_context_get_source_output_info() and friends  
   type pa_source_output_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_source_output_info;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:720

  --* Get information about a source output by its index  
   function pa_context_get_source_output_info
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pa_source_output_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:723
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_source_output_info";

  --* Get the complete list of source outputs  
   function pa_context_get_source_output_info_list
     (c : access pulse_context_h.pa_context;
      cb : pa_source_output_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:726
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_source_output_info_list";

  --* Move the specified source output to a different source. \since 0.9.5  
   function pa_context_move_source_output_by_name
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      source_name : Interfaces.C.Strings.chars_ptr;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:729
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_move_source_output_by_name";

  --* Move the specified source output to a different source. \since 0.9.5  
   function pa_context_move_source_output_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      source_idx : bits_stdint_uintn_h.uint32_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:732
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_move_source_output_by_index";

  --* Set the volume of a source output stream \since 1.0  
   function pa_context_set_source_output_volume
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      volume : access constant pulse_volume_h.pa_cvolume;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:735
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_source_output_volume";

  --* Set the mute switch of a source output stream \since 1.0  
   function pa_context_set_source_output_mute
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      mute : int;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:738
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_source_output_mute";

  --* Kill a source output.  
   function pa_context_kill_source_output
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:741
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_kill_source_output";

  --* @}  
  --* @{ \name Statistics  
  --* Memory block statistics. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  

  --*< Currently allocated memory blocks  
   type pa_stat_info is record
      memblock_total : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:751
      memblock_total_size : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:752
      memblock_allocated : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:753
      memblock_allocated_size : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:754
      scache_size : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:755
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:750

  --*< Current total size of allocated memory blocks  
  --*< Allocated memory blocks during the whole lifetime of the daemon.  
  --*< Total size of all memory blocks allocated during the whole lifetime of the daemon.  
  --*< Total size of all sample cache entries.  
  --* Callback prototype for pa_context_stat()  
   type pa_stat_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_stat_info;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:759

  --* Get daemon memory block statistics  
   function pa_context_stat
     (c : access pulse_context_h.pa_context;
      cb : pa_stat_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:762
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_stat";

  --* @}  
  --* @{ \name Cached Samples  
  --* Stores information about sample cache entries. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  

  --*< Index of this entry  
   type pa_sample_info is record
      index : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:772
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:773
      volume : aliased pulse_volume_h.pa_cvolume;  -- /usr/include/pulse/introspect.h:774
      sample_spec : aliased pulse_sample_h.pa_sample_spec;  -- /usr/include/pulse/introspect.h:775
      channel_map : aliased pulse_channelmap_h.pa_channel_map;  -- /usr/include/pulse/introspect.h:776
      duration : aliased pulse_sample_h.pa_usec_t;  -- /usr/include/pulse/introspect.h:777
      bytes : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:778
      lazy : aliased int;  -- /usr/include/pulse/introspect.h:779
      filename : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:780
      proplist : access pulse_proplist_h.pa_proplist;  -- /usr/include/pulse/introspect.h:781
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:771

  --*< Name of this entry  
  --*< Default volume of this entry  
  --*< Sample specification of the sample  
  --*< The channel map  
  --*< Duration of this entry  
  --*< Length of this sample in bytes.  
  --*< Non-zero when this is a lazy cache entry.  
  --*< In case this is a lazy cache entry, the filename for the sound file to be loaded on demand.  
  --*< Property list for this sample. \since 0.9.11  
  --* Callback prototype for pa_context_get_sample_info_by_name() and friends  
   type pa_sample_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_sample_info;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:785

  --* Get information about a sample by its name  
   function pa_context_get_sample_info_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_sample_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:788
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_sample_info_by_name";

  --* Get information about a sample by its index  
   function pa_context_get_sample_info_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pa_sample_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:791
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_sample_info_by_index";

  --* Get the complete list of samples stored in the daemon.  
   function pa_context_get_sample_info_list
     (c : access pulse_context_h.pa_context;
      cb : pa_sample_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:794
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_sample_info_list";

  --* @}  
  --* \cond fulldocs  
  --* @{ \name Autoload Entries  
  --* \deprecated Type of an autoload entry.  
   type pa_autoload_type is 
     (PA_AUTOLOAD_SINK,
      PA_AUTOLOAD_SOURCE)
   with Convention => C;  -- /usr/include/pulse/introspect.h:803

   subtype pa_autoload_type_t is pa_autoload_type;  -- /usr/include/pulse/introspect.h:806

  --* \deprecated Stores information about autoload entries. Please note that this structure
  -- * can be extended as part of evolutionary API updates at any time in
  -- * any new release.  

  --*< Index of this autoload entry  
   type pa_autoload_info is record
      index : aliased bits_stdint_uintn_h.uint32_t;  -- /usr/include/pulse/introspect.h:812
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:813
      c_type : aliased pa_autoload_type_t;  -- /usr/include/pulse/introspect.h:814
      module : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:815
      argument : Interfaces.C.Strings.chars_ptr;  -- /usr/include/pulse/introspect.h:816
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pulse/introspect.h:811

  --*< Name of the sink or source  
  --*< Type of the autoload entry  
  --*< Module name to load  
  --*< Argument string for module  
  --* \deprecated Callback prototype for pa_context_get_autoload_info_by_name() and friends  
   type pa_autoload_info_cb_t is access procedure
        (arg1 : access pulse_context_h.pa_context;
         arg2 : access constant pa_autoload_info;
         arg3 : int;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/introspect.h:820

  --* \deprecated Get info about a specific autoload entry.  
   function pa_context_get_autoload_info_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      c_type : pa_autoload_type_t;
      cb : pa_autoload_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:823
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_autoload_info_by_name";

  --* \deprecated Get info about a specific autoload entry.  
   function pa_context_get_autoload_info_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pa_autoload_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:826
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_autoload_info_by_index";

  --* \deprecated Get the complete list of autoload entries.  
   function pa_context_get_autoload_info_list
     (c : access pulse_context_h.pa_context;
      cb : pa_autoload_info_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:829
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_autoload_info_list";

  --* \deprecated Add a new autoload entry.  
   function pa_context_add_autoload
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      c_type : pa_autoload_type_t;
      module : Interfaces.C.Strings.chars_ptr;
      argument : Interfaces.C.Strings.chars_ptr;
      arg6 : pa_context_index_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:832
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_add_autoload";

  --* \deprecated Remove an autoload entry.  
   function pa_context_remove_autoload_by_name
     (c : access pulse_context_h.pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      c_type : pa_autoload_type_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:835
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_remove_autoload_by_name";

  --* \deprecated Remove an autoload entry.  
   function pa_context_remove_autoload_by_index
     (c : access pulse_context_h.pa_context;
      idx : bits_stdint_uintn_h.uint32_t;
      cb : pulse_context_h.pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/introspect.h:838
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_remove_autoload_by_index";

  --* @}  
  --* \endcond  
end pulse_introspect_h;
