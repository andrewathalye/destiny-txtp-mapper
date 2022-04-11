pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with pulse_proplist_h;
with pulse_mainloop_api_h;
with pulse_def_h;
limited with pulse_operation_h;
with bits_stdint_uintn_h;
with pulse_sample_h;
with stddef_h;

package pulse_context_h is

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

  --* \page async Asynchronous API
  -- *
  -- * \section overv_sec Overview
  -- *
  -- * The asynchronous API is the native interface to the PulseAudio library.
  -- * It allows full access to all available functionality. This however means that
  -- * it is rather complex and can take some time to fully master.
  -- *
  -- * \section mainloop_sec Main Loop Abstraction
  -- *
  -- * The API is based around an asynchronous event loop, or main loop,
  -- * abstraction. This abstraction contains three basic elements:
  -- *
  -- * \li Deferred events - Events that will trigger as soon as possible. Note
  -- *                       that some implementations may block all other events
  -- *                       when a deferred event is active.
  -- * \li I/O events - Events that trigger on file descriptor activities.
  -- * \li Timer events - Events that trigger after a fixed amount of time.
  -- *
  -- * The abstraction is represented as a number of function pointers in the
  -- * pa_mainloop_api structure.
  -- *
  -- * To actually be able to use these functions, an implementation needs to
  -- * be coupled to the abstraction. There are three of these shipped with
  -- * PulseAudio, but any other can be used with a minimal amount of work,
  -- * provided it supports the three basic events listed above.
  -- *
  -- * The implementations shipped with PulseAudio are:
  -- *
  -- * \li \subpage mainloop - A minimal but fast implementation based on poll().
  -- * \li \subpage threaded_mainloop - A special version of the previous
  -- *                                  implementation where all of PulseAudio's
  -- *                                  internal handling runs in a separate
  -- *                                  thread.
  -- * \li \subpage glib-mainloop - A wrapper around GLib's main loop.
  -- *
  -- * UNIX signals may be hooked to a main loop using the functions from
  -- * \ref mainloop-signal.h. These rely only on the main loop abstraction
  -- * and can therefore be used with any of the implementations.
  -- *
  -- * \section refcnt_sec Reference Counting
  -- *
  -- * Almost all objects in PulseAudio are reference counted. What that means
  -- * is that you rarely malloc() or free() any objects. Instead you increase
  -- * and decrease their reference counts. Whenever an object's reference
  -- * count reaches zero, that object gets destroy and any resources it uses
  -- * get freed.
  -- *
  -- * The benefit of this design is that an application need not worry about
  -- * whether or not it needs to keep an object around in case the library is
  -- * using it internally. If it is, then it has made sure it has its own
  -- * reference to it.
  -- *
  -- * Whenever the library creates an object, it will have an initial
  -- * reference count of one. Most of the time, this single reference will be
  -- * sufficient for the application, so all required reference count
  -- * interaction will be a single call to the object's unref function.
  -- *
  -- * \section context_sec Context
  -- *
  -- * A context is the basic object for a connection to a PulseAudio server.
  -- * It multiplexes commands, data streams and events through a single
  -- * channel.
  -- *
  -- * There is no need for more than one context per application, unless
  -- * connections to multiple servers are needed.
  -- *
  -- * \subsection ops_subsec Operations
  -- *
  -- * All operations on the context are performed asynchronously. I.e. the
  -- * client will not wait for the server to complete the request. To keep
  -- * track of all these in-flight operations, the application is given a
  -- * pa_operation object for each asynchronous operation.
  -- *
  -- * There are only two actions (besides reference counting) that can be
  -- * performed on a pa_operation: querying its state with
  -- * pa_operation_get_state() and aborting it with pa_operation_cancel().
  -- *
  -- * A pa_operation object is reference counted, so an application must
  -- * make sure to unreference it, even if it has no intention of using it.
  -- *
  -- * \subsection conn_subsec Connecting
  -- *
  -- * A context must be connected to a server before any operation can be
  -- * issued. Calling pa_context_connect() will initiate the connection
  -- * procedure. Unlike most asynchronous operations, connecting does not
  -- * result in a pa_operation object. Instead, the application should
  -- * register a callback using pa_context_set_state_callback().
  -- *
  -- * \subsection disc_subsec Disconnecting
  -- *
  -- * When the sound support is no longer needed, the connection needs to be
  -- * closed using pa_context_disconnect(). This is an immediate function that
  -- * works synchronously.
  -- *
  -- * Since the context object has references to other objects it must be
  -- * disconnected after use or there is a high risk of memory leaks. If the
  -- * connection has terminated by itself, then there is no need to explicitly
  -- * disconnect the context using pa_context_disconnect().
  -- *
  -- * \section Functions
  -- *
  -- * The sound server's functionality can be divided into a number of
  -- * subsections:
  -- *
  -- * \li \subpage streams
  -- * \li \subpage scache
  -- * \li \subpage introspect
  -- * \li \subpage subscribe
  --  

  --* \file
  -- * Connection contexts for asynchronous communication with a
  -- * server. A pa_context object wraps a connection to a PulseAudio
  -- * server using its native protocol.
  -- *
  -- * See also \subpage async
  --  

  --* An opaque connection context to a daemon  
   type pa_context is null record;   -- incomplete struct

  --* Generic notification callback prototype  
   type pa_context_notify_cb_t is access procedure (arg1 : access pa_context; arg2 : System.Address)
   with Convention => C;  -- /usr/include/pulse/context.h:157

  --* A generic callback for operation completion  
   type pa_context_success_cb_t is access procedure
        (arg1 : access pa_context;
         arg2 : int;
         arg3 : System.Address)
   with Convention => C;  -- /usr/include/pulse/context.h:160

  --* A callback for asynchronous meta/policy event messages. The set
  -- * of defined events can be extended at any time. Also, server modules
  -- * may introduce additional message types so make sure that your
  -- * callback function ignores messages it doesn't know. \since
  -- * 0.9.15  

   type pa_context_event_cb_t is access procedure
        (arg1 : access pa_context;
         arg2 : Interfaces.C.Strings.chars_ptr;
         arg3 : access pulse_proplist_h.pa_proplist;
         arg4 : System.Address)
   with Convention => C;  -- /usr/include/pulse/context.h:167

  --* Instantiate a new connection context with an abstract mainloop API
  -- * and an application name. It is recommended to use pa_context_new_with_proplist()
  -- * instead and specify some initial properties. 

   function pa_context_new (mainloop : access pulse_mainloop_api_h.pa_mainloop_api; name : Interfaces.C.Strings.chars_ptr) return access pa_context  -- /usr/include/pulse/context.h:172
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_new";

  --* Instantiate a new connection context with an abstract mainloop API
  -- * and an application name, and specify the initial client property
  -- * list. \since 0.9.11  

   function pa_context_new_with_proplist
     (mainloop : access pulse_mainloop_api_h.pa_mainloop_api;
      name : Interfaces.C.Strings.chars_ptr;
      proplist : access constant pulse_proplist_h.pa_proplist) return access pa_context  -- /usr/include/pulse/context.h:177
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_new_with_proplist";

  --* Decrease the reference counter of the context by one  
   procedure pa_context_unref (c : access pa_context)  -- /usr/include/pulse/context.h:180
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_unref";

  --* Increase the reference counter of the context by one  
   function pa_context_ref (c : access pa_context) return access pa_context  -- /usr/include/pulse/context.h:183
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_ref";

  --* Set a callback function that is called whenever the context status changes  
   procedure pa_context_set_state_callback
     (c : access pa_context;
      cb : pa_context_notify_cb_t;
      userdata : System.Address)  -- /usr/include/pulse/context.h:186
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_state_callback";

  --* Set a callback function that is called whenever a meta/policy
  -- * control event is received. \since 0.9.15  

   procedure pa_context_set_event_callback
     (p : access pa_context;
      cb : pa_context_event_cb_t;
      userdata : System.Address)  -- /usr/include/pulse/context.h:190
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_event_callback";

  --* Return the error number of the last failed operation  
   function pa_context_errno (c : access constant pa_context) return int  -- /usr/include/pulse/context.h:193
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_errno";

  --* Return non-zero if some data is pending to be written to the connection  
   function pa_context_is_pending (c : access constant pa_context) return int  -- /usr/include/pulse/context.h:196
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_is_pending";

  --* Return the current context status  
   function pa_context_get_state (c : access constant pa_context) return pulse_def_h.pa_context_state_t  -- /usr/include/pulse/context.h:199
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_state";

  --* Connect the context to the specified server. If server is NULL,
  -- * connect to the default server. This routine may but will not always
  -- * return synchronously on error. Use pa_context_set_state_callback() to
  -- * be notified when the connection is established. If flags doesn't have
  -- * PA_CONTEXT_NOAUTOSPAWN set and no specific server is specified or
  -- * accessible a new daemon is spawned. If api is non-NULL, the functions
  -- * specified in the structure are used when forking a new child
  -- * process. Returns negative on certain errors such as invalid state
  -- * or parameters.  

   function pa_context_connect
     (c : access pa_context;
      server : Interfaces.C.Strings.chars_ptr;
      flags : pulse_def_h.pa_context_flags_t;
      api : access constant pulse_def_h.pa_spawn_api) return int  -- /usr/include/pulse/context.h:210
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_connect";

  --* Terminate the context connection immediately  
   procedure pa_context_disconnect (c : access pa_context)  -- /usr/include/pulse/context.h:213
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_disconnect";

  --* Drain the context. If there is nothing to drain, the function returns NULL  
   function pa_context_drain
     (c : access pa_context;
      cb : pa_context_notify_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/context.h:216
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_drain";

  --* Tell the daemon to exit. The returned operation is unlikely to
  -- * complete successfully, since the daemon probably died before
  -- * returning a success notification  

   function pa_context_exit_daemon
     (c : access pa_context;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/context.h:221
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_exit_daemon";

  --* Set the name of the default sink.  
   function pa_context_set_default_sink
     (c : access pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/context.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_default_sink";

  --* Set the name of the default source.  
   function pa_context_set_default_source
     (c : access pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/context.h:227
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_default_source";

  --* Returns 1 when the connection is to a local daemon. Returns negative when no connection has been made yet.  
   function pa_context_is_local (c : access constant pa_context) return int  -- /usr/include/pulse/context.h:230
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_is_local";

  --* Set a different application name for context on the server.  
   function pa_context_set_name
     (c : access pa_context;
      name : Interfaces.C.Strings.chars_ptr;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/context.h:233
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_set_name";

  --* Return the server name this context is connected to.  
   function pa_context_get_server (c : access constant pa_context) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/context.h:236
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_server";

  --* Return the protocol version of the library.  
   function pa_context_get_protocol_version (c : access constant pa_context) return bits_stdint_uintn_h.uint32_t  -- /usr/include/pulse/context.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_protocol_version";

  --* Return the protocol version of the connected server.
  -- * Returns PA_INVALID_INDEX on error.  

   function pa_context_get_server_protocol_version (c : access constant pa_context) return bits_stdint_uintn_h.uint32_t  -- /usr/include/pulse/context.h:243
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_server_protocol_version";

  --* Update the property list of the client, adding new entries. Please
  -- * note that it is highly recommended to set as many properties
  -- * initially via pa_context_new_with_proplist() as possible instead a
  -- * posteriori with this function, since that information may then be
  -- * used to route streams of the client to the right device. \since 0.9.11  

   function pa_context_proplist_update
     (c : access pa_context;
      mode : pulse_proplist_h.pa_update_mode_t;
      p : access constant pulse_proplist_h.pa_proplist;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/context.h:250
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_proplist_update";

  --* Update the property list of the client, remove entries. \since 0.9.11  
   function pa_context_proplist_remove
     (c : access pa_context;
      keys : System.Address;
      cb : pa_context_success_cb_t;
      userdata : System.Address) return access pulse_operation_h.pa_operation  -- /usr/include/pulse/context.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_proplist_remove";

  --* Return the client index this context is
  -- * identified in the server with. This is useful for usage with the
  -- * introspection functions, such as pa_context_get_client_info().
  -- * Returns PA_INVALID_INDEX on error. \since 0.9.11  

   function pa_context_get_index (s : access constant pa_context) return bits_stdint_uintn_h.uint32_t  -- /usr/include/pulse/context.h:259
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_index";

  --* Create a new timer event source for the specified time (wrapper
  -- * for mainloop->time_new). \since 0.9.16  

   function pa_context_rttime_new
     (c : access constant pa_context;
      usec : pulse_sample_h.pa_usec_t;
      cb : pulse_mainloop_api_h.pa_time_event_cb_t;
      userdata : System.Address) return access pulse_mainloop_api_h.pa_time_event  -- /usr/include/pulse/context.h:263
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_rttime_new";

  --* Restart a running or expired timer event source (wrapper for
  -- * mainloop->time_restart). \since 0.9.16  

   procedure pa_context_rttime_restart
     (c : access constant pa_context;
      e : access pulse_mainloop_api_h.pa_time_event;
      usec : pulse_sample_h.pa_usec_t)  -- /usr/include/pulse/context.h:267
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_rttime_restart";

  --* Return the optimal block size for passing around audio buffers. It
  -- * is recommended to allocate buffers of the size returned here when
  -- * writing audio data to playback streams, if the latency constraints
  -- * permit this. It is not recommended writing larger blocks than this
  -- * because usually they will then be split up internally into chunks
  -- * of this size. It is not recommended writing smaller blocks than
  -- * this (unless required due to latency demands) because this
  -- * increases CPU usage. If ss is NULL you will be returned the
  -- * byte-exact tile size. if ss is invalid, (size_t) -1 will be
  -- * returned. If you pass a valid ss, then the tile size
  -- * will be rounded down to multiple of the frame size. This is
  -- * supposed to be used in a construct such as
  -- * pa_context_get_tile_size(pa_stream_get_context(s),
  -- * pa_stream_get_sample_spec(ss)); \since 0.9.20  

   function pa_context_get_tile_size (c : access constant pa_context; ss : access constant pulse_sample_h.pa_sample_spec) return stddef_h.size_t  -- /usr/include/pulse/context.h:283
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_get_tile_size";

  --* Load the authentication cookie from a file. This function is primarily
  -- * meant for PulseAudio's own tunnel modules, which need to load the cookie
  -- * from a custom location. Applications don't usually need to care about the
  -- * cookie at all, but if it happens that you know what the authentication
  -- * cookie is and your application needs to load it from a non-standard
  -- * location, feel free to use this function. \since 5.0  

   function pa_context_load_cookie_from_file (c : access pa_context; cookie_file_path : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/pulse/context.h:291
   with Import => True, 
        Convention => C, 
        External_Name => "pa_context_load_cookie_from_file";

end pulse_context_h;
