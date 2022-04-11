pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with stddef_h;

package pulse_proplist_h is

   PA_PROP_MEDIA_NAME : aliased constant String := "media.name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:35

   PA_PROP_MEDIA_TITLE : aliased constant String := "media.title" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:38

   PA_PROP_MEDIA_ARTIST : aliased constant String := "media.artist" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:41

   PA_PROP_MEDIA_COPYRIGHT : aliased constant String := "media.copyright" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:44

   PA_PROP_MEDIA_SOFTWARE : aliased constant String := "media.software" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:47

   PA_PROP_MEDIA_LANGUAGE : aliased constant String := "media.language" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:50

   PA_PROP_MEDIA_FILENAME : aliased constant String := "media.filename" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:53

   PA_PROP_MEDIA_ICON : aliased constant String := "media.icon" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:57

   PA_PROP_MEDIA_ICON_NAME : aliased constant String := "media.icon_name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:61

   PA_PROP_MEDIA_ROLE : aliased constant String := "media.role" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:64

   PA_PROP_FILTER_WANT : aliased constant String := "filter.want" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:67

   PA_PROP_FILTER_APPLY : aliased constant String := "filter.apply" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:70

   PA_PROP_FILTER_SUPPRESS : aliased constant String := "filter.suppress" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:73

   PA_PROP_EVENT_ID : aliased constant String := "event.id" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:76

   PA_PROP_EVENT_DESCRIPTION : aliased constant String := "event.description" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:79

   PA_PROP_EVENT_MOUSE_X : aliased constant String := "event.mouse.x" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:82

   PA_PROP_EVENT_MOUSE_Y : aliased constant String := "event.mouse.y" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:85

   PA_PROP_EVENT_MOUSE_HPOS : aliased constant String := "event.mouse.hpos" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:88

   PA_PROP_EVENT_MOUSE_VPOS : aliased constant String := "event.mouse.vpos" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:91

   PA_PROP_EVENT_MOUSE_BUTTON : aliased constant String := "event.mouse.button" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:94

   PA_PROP_WINDOW_NAME : aliased constant String := "window.name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:97

   PA_PROP_WINDOW_ID : aliased constant String := "window.id" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:100

   PA_PROP_WINDOW_ICON : aliased constant String := "window.icon" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:104

   PA_PROP_WINDOW_ICON_NAME : aliased constant String := "window.icon_name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:108

   PA_PROP_WINDOW_X : aliased constant String := "window.x" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:111

   PA_PROP_WINDOW_Y : aliased constant String := "window.y" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:114

   PA_PROP_WINDOW_WIDTH : aliased constant String := "window.width" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:117

   PA_PROP_WINDOW_HEIGHT : aliased constant String := "window.height" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:120

   PA_PROP_WINDOW_HPOS : aliased constant String := "window.hpos" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:123

   PA_PROP_WINDOW_VPOS : aliased constant String := "window.vpos" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:126

   PA_PROP_WINDOW_DESKTOP : aliased constant String := "window.desktop" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:129

   PA_PROP_WINDOW_X11_DISPLAY : aliased constant String := "window.x11.display" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:132

   PA_PROP_WINDOW_X11_SCREEN : aliased constant String := "window.x11.screen" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:135

   PA_PROP_WINDOW_X11_MONITOR : aliased constant String := "window.x11.monitor" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:138

   PA_PROP_WINDOW_X11_XID : aliased constant String := "window.x11.xid" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:141

   PA_PROP_APPLICATION_NAME : aliased constant String := "application.name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:144

   PA_PROP_APPLICATION_ID : aliased constant String := "application.id" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:147

   PA_PROP_APPLICATION_VERSION : aliased constant String := "application.version" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:150

   PA_PROP_APPLICATION_ICON : aliased constant String := "application.icon" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:154

   PA_PROP_APPLICATION_ICON_NAME : aliased constant String := "application.icon_name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:158

   PA_PROP_APPLICATION_LANGUAGE : aliased constant String := "application.language" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:161

   PA_PROP_APPLICATION_PROCESS_ID : aliased constant String := "application.process.id" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:164

   PA_PROP_APPLICATION_PROCESS_BINARY : aliased constant String := "application.process.binary" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:167

   PA_PROP_APPLICATION_PROCESS_USER : aliased constant String := "application.process.user" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:170

   PA_PROP_APPLICATION_PROCESS_HOST : aliased constant String := "application.process.host" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:173

   PA_PROP_APPLICATION_PROCESS_MACHINE_ID : aliased constant String := "application.process.machine_id" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:176

   PA_PROP_APPLICATION_PROCESS_SESSION_ID : aliased constant String := "application.process.session_id" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:179

   PA_PROP_DEVICE_STRING : aliased constant String := "device.string" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:182

   PA_PROP_DEVICE_API : aliased constant String := "device.api" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:185

   PA_PROP_DEVICE_DESCRIPTION : aliased constant String := "device.description" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:188

   PA_PROP_DEVICE_BUS_PATH : aliased constant String := "device.bus_path" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:191

   PA_PROP_DEVICE_SERIAL : aliased constant String := "device.serial" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:194

   PA_PROP_DEVICE_VENDOR_ID : aliased constant String := "device.vendor.id" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:197

   PA_PROP_DEVICE_VENDOR_NAME : aliased constant String := "device.vendor.name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:200

   PA_PROP_DEVICE_PRODUCT_ID : aliased constant String := "device.product.id" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:203

   PA_PROP_DEVICE_PRODUCT_NAME : aliased constant String := "device.product.name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:206

   PA_PROP_DEVICE_CLASS : aliased constant String := "device.class" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:209

   PA_PROP_DEVICE_FORM_FACTOR : aliased constant String := "device.form_factor" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:212

   PA_PROP_DEVICE_BUS : aliased constant String := "device.bus" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:215

   PA_PROP_DEVICE_ICON : aliased constant String := "device.icon" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:219

   PA_PROP_DEVICE_ICON_NAME : aliased constant String := "device.icon_name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:223

   PA_PROP_DEVICE_ACCESS_MODE : aliased constant String := "device.access_mode" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:226

   PA_PROP_DEVICE_MASTER_DEVICE : aliased constant String := "device.master_device" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:229

   PA_PROP_DEVICE_BUFFERING_BUFFER_SIZE : aliased constant String := "device.buffering.buffer_size" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:232

   PA_PROP_DEVICE_BUFFERING_FRAGMENT_SIZE : aliased constant String := "device.buffering.fragment_size" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:235

   PA_PROP_DEVICE_PROFILE_NAME : aliased constant String := "device.profile.name" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:238

   PA_PROP_DEVICE_INTENDED_ROLES : aliased constant String := "device.intended_roles" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:241

   PA_PROP_DEVICE_PROFILE_DESCRIPTION : aliased constant String := "device.profile.description" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:244

   PA_PROP_MODULE_AUTHOR : aliased constant String := "module.author" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:247

   PA_PROP_MODULE_DESCRIPTION : aliased constant String := "module.description" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:250

   PA_PROP_MODULE_USAGE : aliased constant String := "module.usage" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:253

   PA_PROP_MODULE_VERSION : aliased constant String := "module.version" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:256

   PA_PROP_FORMAT_SAMPLE_FORMAT : aliased constant String := "format.sample_format" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:259

   PA_PROP_FORMAT_RATE : aliased constant String := "format.rate" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:262

   PA_PROP_FORMAT_CHANNELS : aliased constant String := "format.channels" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:265

   PA_PROP_FORMAT_CHANNEL_MAP : aliased constant String := "format.channel_map" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:268

   PA_PROP_CONTEXT_FORCE_DISABLE_SHM : aliased constant String := "context.force.disable.shm" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:271

   PA_PROP_BLUETOOTH_CODEC : aliased constant String := "bluetooth.codec" & ASCII.NUL;  --  /usr/include/pulse/proplist.h:274
   --  unsupported macro: PA_UPDATE_SET PA_UPDATE_SET
   --  unsupported macro: PA_UPDATE_MERGE PA_UPDATE_MERGE
   --  unsupported macro: PA_UPDATE_REPLACE PA_UPDATE_REPLACE

  --**
  --  This file is part of PulseAudio.
  --  Copyright 2007 Lennart Poettering
  --  PulseAudio is free software; you can redistribute it and/or modify
  --  it under the terms of the GNU Lesser General Public License as
  --  published by the Free Software Foundation; either version 2.1 of the
  --  License, or (at your option) any later version.
  --  PulseAudio is distributed in the hope that it will be useful, but
  --  WITHOUT ANY WARRANTY; without even the implied warranty of
  --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  --  Lesser General Public License for more details.
  --  You should have received a copy of the GNU Lesser General Public
  --  License along with PulseAudio; if not, see <http://www.gnu.org/licenses/>.
  --** 

  --* \file
  -- * Property list constants and functions  

  --* For streams: localized media name, formatted as UTF-8. E.g. "Guns'N'Roses: Civil War". 
  --* For streams: localized media title if applicable, formatted as UTF-8. E.g. "Civil War"  
  --* For streams: localized media artist if applicable, formatted as UTF-8. E.g. "Guns'N'Roses"  
  --* For streams: localized media copyright string if applicable, formatted as UTF-8. E.g. "Evil Record Corp."  
  --* For streams: localized media generator software string if applicable, formatted as UTF-8. E.g. "Foocrop AudioFrobnicator"  
  --* For streams: media language if applicable, in standard POSIX format. E.g. "de_DE"  
  --* For streams: source filename if applicable, in URI format or local path. E.g. "/home/lennart/music/foobar.ogg"  
  --* \cond fulldocs  
  --* For streams: icon for the media. A binary blob containing PNG image data  
  --* \endcond  
  --* For streams: an XDG icon name for the media. E.g. "audio-x-mp3"  
  --* For streams: logic role of this media. One of the strings "video", "music", "game", "event", "phone", "animation", "production", "a11y", "test"  
  --* For streams: the name of a filter that is desired, e.g.\ "echo-cancel" or "equalizer-sink". PulseAudio may choose to not apply the filter if it does not make sense (for example, applying echo-cancellation on a Bluetooth headset probably does not make sense. \since 1.0  
  --* For streams: the name of a filter that is desired, e.g.\ "echo-cancel" or "equalizer-sink". Differs from PA_PROP_FILTER_WANT in that it forces PulseAudio to apply the filter, regardless of whether PulseAudio thinks it makes sense to do so or not. If this is set, PA_PROP_FILTER_WANT is ignored. In other words, you almost certainly do not want to use this. \since 1.0  
  --* For streams: the name of a filter that should specifically suppressed (i.e.\ overrides PA_PROP_FILTER_WANT). Useful for the times that PA_PROP_FILTER_WANT is automatically added (e.g. echo-cancellation for phone streams when $VOIP_APP does its own, internal AEC) \since 1.0  
  --* For event sound streams: XDG event sound name. e.g.\ "message-new-email" (Event sound streams are those with media.role set to "event")  
  --* For event sound streams: localized human readable one-line description of the event, formatted as UTF-8. E.g. "Email from lennart@example.com received."  
  --* For event sound streams: absolute horizontal mouse position on the screen if the event sound was triggered by a mouse click, integer formatted as text string. E.g. "865"  
  --* For event sound streams: absolute vertical mouse position on the screen if the event sound was triggered by a mouse click, integer formatted as text string. E.g. "432"  
  --* For event sound streams: relative horizontal mouse position on the screen if the event sound was triggered by a mouse click, float formatted as text string, ranging from 0.0 (left side of the screen) to 1.0 (right side of the screen). E.g. "0.65"  
  --* For event sound streams: relative vertical mouse position on the screen if the event sound was triggered by a mouse click, float formatted as text string, ranging from 0.0 (top of the screen) to 1.0 (bottom of the screen). E.g. "0.43"  
  --* For event sound streams: mouse button that triggered the event if applicable, integer formatted as string with 0=left, 1=middle, 2=right. E.g. "0"  
  --* For streams that belong to a window on the screen: localized window title. E.g. "Totem Music Player"  
  --* For streams that belong to a window on the screen: a textual id for identifying a window logically. E.g. "org.gnome.Totem.MainWindow"  
  --* \cond fulldocs  
  --* For streams that belong to a window on the screen: window icon. A binary blob containing PNG image data  
  --* \endcond  
  --* For streams that belong to a window on the screen: an XDG icon name for the window. E.g. "totem"  
  --* For streams that belong to a window on the screen: absolute horizontal window position on the screen, integer formatted as text string. E.g. "865". \since 0.9.17  
  --* For streams that belong to a window on the screen: absolute vertical window position on the screen, integer formatted as text string. E.g. "343". \since 0.9.17  
  --* For streams that belong to a window on the screen: window width on the screen, integer formatted as text string. e.g. "365". \since 0.9.17  
  --* For streams that belong to a window on the screen: window height on the screen, integer formatted as text string. E.g. "643". \since 0.9.17  
  --* For streams that belong to a window on the screen: relative position of the window center on the screen, float formatted as text string, ranging from 0.0 (left side of the screen) to 1.0 (right side of the screen). E.g. "0.65". \since 0.9.17  
  --* For streams that belong to a window on the screen: relative position of the window center on the screen, float formatted as text string, ranging from 0.0 (top of the screen) to 1.0 (bottom of the screen). E.g. "0.43". \since 0.9.17  
  --* For streams that belong to a window on the screen: if the windowing system supports multiple desktops, a comma separated list of indexes of the desktops this window is visible on. If this property is an empty string, it is visible on all desktops (i.e. 'sticky'). The first desktop is 0. E.g. "0,2,3" \since 0.9.18  
  --* For streams that belong to an X11 window on the screen: the X11 display string. E.g. ":0.0"  
  --* For streams that belong to an X11 window on the screen: the X11 screen the window is on, an integer formatted as string. E.g. "0"  
  --* For streams that belong to an X11 window on the screen: the X11 monitor the window is on, an integer formatted as string. E.g. "0"  
  --* For streams that belong to an X11 window on the screen: the window XID, an integer formatted as string. E.g. "25632"  
  --* For clients/streams: localized human readable application name. E.g. "Totem Music Player"  
  --* For clients/streams: a textual id for identifying an application logically. E.g. "org.gnome.Totem"  
  --* For clients/streams: a version string, e.g.\ "0.6.88"  
  --* \cond fulldocs  
  --* For clients/streams: application icon. A binary blob containing PNG image data  
  --* \endcond  
  --* For clients/streams: an XDG icon name for the application. E.g. "totem"  
  --* For clients/streams: application language if applicable, in standard POSIX format. E.g. "de_DE"  
  --* For clients/streams on UNIX: application process PID, an integer formatted as string. E.g. "4711"  
  --* For clients/streams: application process name. E.g. "totem"  
  --* For clients/streams: application user name. E.g. "lennart"  
  --* For clients/streams: host name the application runs on. E.g. "omega"  
  --* For clients/streams: the D-Bus host id the application runs on. E.g. "543679e7b01393ed3e3e650047d78f6e"  
  --* For clients/streams: an id for the login session the application runs in. On Unix the value of $XDG_SESSION_ID. E.g. "5"  
  --* For devices: device string in the underlying audio layer's format. E.g. "surround51:0"  
  --* For devices: API this device is access with. E.g. "alsa"  
  --* For devices: localized human readable device one-line description. E.g. "Foobar Industries USB Headset 2000+ Ultra"  
  --* For devices: bus path to the device in the OS' format. E.g. "/sys/bus/pci/devices/0000:00:1f.2"  
  --* For devices: serial number if applicable. E.g. "4711-0815-1234"  
  --* For devices: vendor ID if applicable. E.g. 1274  
  --* For devices: vendor name if applicable. E.g. "Foocorp Heavy Industries"  
  --* For devices: product ID if applicable. E.g. 4565  
  --* For devices: product name if applicable. E.g. "SuperSpeakers 2000 Pro"  
  --* For devices: device class. One of "sound", "modem", "monitor", "filter"  
  --* For devices: form factor if applicable. One of "internal", "speaker", "handset", "tv", "webcam", "microphone", "headset", "headphone", "hands-free", "car", "hifi", "computer", "portable"  
  --* For devices: bus of the device if applicable. One of "isa", "pci", "usb", "firewire", "bluetooth"  
  --* \cond fulldocs  
  --* For devices: icon for the device. A binary blob containing PNG image data  
  --* \endcond  
  --* For devices: an XDG icon name for the device. E.g. "sound-card-speakers-usb"  
  --* For devices: access mode of the device if applicable. One of "mmap", "mmap_rewrite", "serial"  
  --* For filter devices: master device id if applicable.  
  --* For devices: buffer size in bytes, integer formatted as string.  
  --* For devices: fragment size in bytes, integer formatted as string.  
  --* For devices: profile identifier for the profile this devices is in. E.g. "analog-stereo", "analog-surround-40", "iec958-stereo", ... 
  --* For devices: intended use. A space separated list of roles (see PA_PROP_MEDIA_ROLE) this device is particularly well suited for, due to latency, quality or form factor. \since 0.9.16  
  --* For devices: human readable one-line description of the profile this device is in. E.g. "Analog Stereo", ...  
  --* For modules: the author's name, formatted as UTF-8 string. E.g. "Lennart Poettering"  
  --* For modules: a human readable one-line description of the module's purpose formatted as UTF-8. E.g. "Frobnicate sounds with a flux compensator"  
  --* For modules: a human readable usage description of the module's arguments formatted as UTF-8.  
  --* For modules: a version string for the module. E.g. "0.9.15"  
  --* For PCM formats: the sample format used as returned by pa_sample_format_to_string() \since 1.0  
  --* For all formats: the sample rate (unsigned integer) \since 1.0  
  --* For all formats: the number of channels (unsigned integer) \since 1.0  
  --* For PCM formats: the channel map of the stream as returned by pa_channel_map_snprint() \since 1.0  
  --* For context: whether to forcefully disable data transfer via POSIX or memfd shared memory. This property overrides any other client configuration which would otherwise enable SHM communication channels. \since 15.0  
  --* For a bluez device: the currently selected codec name. \since 15.0  
  --* A property list object. Basically a dictionary with ASCII strings
  -- * as keys and arbitrary data as values. \since 0.9.11  

   type pa_proplist is null record;   -- incomplete struct

  --* Allocate a property list. Free with pa_proplist_free. \since 0.9.11  
   function pa_proplist_new return access pa_proplist  -- /usr/include/pulse/proplist.h:281
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_new";

  --* Free the property list. \since 0.9.11  
   procedure pa_proplist_free (p : access pa_proplist)  -- /usr/include/pulse/proplist.h:284
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_free";

  --* Returns a non-zero value if the key is valid. \since 3.0  
   function pa_proplist_key_valid (key : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/pulse/proplist.h:287
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_key_valid";

  --* Append a new string entry to the property list, possibly
  -- * overwriting an already existing entry with the same key. An
  -- * internal copy of the data passed is made. Will accept only valid
  -- * UTF-8. Returns zero on success. \since 0.9.11  

   function pa_proplist_sets
     (p : access pa_proplist;
      key : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/pulse/proplist.h:293
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_sets";

  --* Append a new string entry to the property list, possibly
  -- * overwriting an already existing entry with the same key. An
  -- * internal copy of the data passed is made. Will accept only valid
  -- * UTF-8. The string passed in must contain a '='. Left hand side of
  -- * the '=' is used as key name, the right hand side as string
  -- * data. Returns zero on success. \since 0.9.16  

   function pa_proplist_setp (p : access pa_proplist; pair : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/pulse/proplist.h:301
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_setp";

  --* Append a new string entry to the property list, possibly
  -- * overwriting an already existing entry with the same key. An
  -- * internal copy of the data passed is made. Will accept only valid
  -- * UTF-8. The data can be passed as printf()-style format string with
  -- * arguments. Returns zero on success. \since 0.9.11  

   function pa_proplist_setf
     (p : access pa_proplist;
      key : Interfaces.C.Strings.chars_ptr;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int  -- /usr/include/pulse/proplist.h:308
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_setf";

  --* Append a new arbitrary data entry to the property list, possibly
  -- * overwriting an already existing entry with the same key. An
  -- * internal copy of the data passed is made.
  -- * Returns zero on success. \since 0.9.11  

   function pa_proplist_set
     (p : access pa_proplist;
      key : Interfaces.C.Strings.chars_ptr;
      data : System.Address;
      nbytes : stddef_h.size_t) return int  -- /usr/include/pulse/proplist.h:314
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_set";

  --* Return a string entry for the specified key. Will return NULL if
  -- * the data is not valid UTF-8. Will return a NUL-terminated string in
  -- * an internally allocated buffer. The caller should make a copy of
  -- * the data before accessing the property list again. \since 0.9.11  

   function pa_proplist_gets (p : access constant pa_proplist; key : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/proplist.h:320
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_gets";

  --* Store the value for the specified key in \a data. Will store a
  -- * NUL-terminated string for string entries. The \a data pointer returned will
  -- * point to an internally allocated buffer. The caller should make a
  -- * copy of the data before any subsequent modification or destruction
  -- * of the property list.
  -- * Returns zero on success, negative on error. \since 0.9.11  

   function pa_proplist_get
     (p : access constant pa_proplist;
      key : Interfaces.C.Strings.chars_ptr;
      data : System.Address;
      nbytes : access stddef_h.size_t) return int  -- /usr/include/pulse/proplist.h:328
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_get";

  --* Update mode enum for pa_proplist_update(). \since 0.9.11  
   type pa_update_mode is 
     (PA_UPDATE_SET,
      PA_UPDATE_MERGE,
      PA_UPDATE_REPLACE)
   with Convention => C;  -- /usr/include/pulse/proplist.h:331

  --*< Replace the entire property list with the new one. Don't keep
  --     *  any of the old data around.  

  --*< Merge new property list into the existing one, not replacing
  --     *  any old entries if they share a common key with the new
  --     *  property list.  

  --*< Merge new property list into the existing one, replacing all
  --     *  old entries that share a common key with the new property
  --     *  list.  

   subtype pa_update_mode_t is pa_update_mode;  -- /usr/include/pulse/proplist.h:345

  --* \cond fulldocs  
  --* \endcond  
  --* Merge property list "other" into "p", adhering the merge mode as
  -- * specified in "mode". \since 0.9.11  

   procedure pa_proplist_update
     (p : access pa_proplist;
      mode : pa_update_mode_t;
      other : access constant pa_proplist)  -- /usr/include/pulse/proplist.h:355
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_update";

  --* Removes a single entry from the property list, identified be the
  -- * specified key name. Returns zero on success, negative on error.
  -- * \since 0.9.11  

   function pa_proplist_unset (p : access pa_proplist; key : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/pulse/proplist.h:360
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_unset";

  --* Similar to pa_proplist_unset() but takes an array of keys to
  -- * remove. The array should be terminated by a NULL pointer. Returns -1
  -- * on failure, otherwise the number of entries actually removed (which
  -- * might even be 0, if there were no matching entries to
  -- * remove). \since 0.9.11  

   function pa_proplist_unset_many (p : access pa_proplist; keys : System.Address) return int  -- /usr/include/pulse/proplist.h:367
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_unset_many";

  --* Iterate through the property list. The user should allocate a
  -- * state variable of type void* and initialize it with NULL. A pointer
  -- * to this variable should then be passed to pa_proplist_iterate()
  -- * which should be called in a loop until it returns NULL which
  -- * signifies EOL. The property list should not be modified during
  -- * iteration through the list -- with the exception of deleting the
  -- * current entry. On each invocation this function will return the
  -- * key string for the next entry. The keys in the property list do not
  -- * have any particular order. \since 0.9.11  

   function pa_proplist_iterate (p : access constant pa_proplist; state : System.Address) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/proplist.h:378
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_iterate";

  --* Format the property list nicely as a human readable string. This
  -- * works very much like pa_proplist_to_string_sep() and uses a newline
  -- * as separator and appends one final one. Call pa_xfree() on the
  -- * result. \since 0.9.11  

   function pa_proplist_to_string (p : access constant pa_proplist) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/proplist.h:384
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_to_string";

  --* Format the property list nicely as a human readable string and
  -- * choose the separator. Call pa_xfree() on the result. \since
  -- * 0.9.15  

   function pa_proplist_to_string_sep (p : access constant pa_proplist; sep : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr  -- /usr/include/pulse/proplist.h:389
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_to_string_sep";

  --* Allocate a new property list and assign key/value from a human
  -- * readable string. \since 0.9.15  

   function pa_proplist_from_string (str : Interfaces.C.Strings.chars_ptr) return access pa_proplist  -- /usr/include/pulse/proplist.h:393
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_from_string";

  --* Returns 1 if an entry for the specified key exists in the
  -- * property list. Returns negative on error. \since 0.9.11  

   function pa_proplist_contains (p : access constant pa_proplist; key : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/pulse/proplist.h:397
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_contains";

  --* Remove all entries from the property list object. \since 0.9.11  
   procedure pa_proplist_clear (p : access pa_proplist)  -- /usr/include/pulse/proplist.h:400
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_clear";

  --* Allocate a new property list and copy over every single entry from
  -- * the specified list. \since 0.9.11  

   function pa_proplist_copy (p : access constant pa_proplist) return access pa_proplist  -- /usr/include/pulse/proplist.h:404
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_copy";

  --* Return the number of entries in the property list. \since 0.9.15  
   function pa_proplist_size (p : access constant pa_proplist) return unsigned  -- /usr/include/pulse/proplist.h:407
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_size";

  --* Returns 0 when the proplist is empty, positive otherwise \since 0.9.15  
   function pa_proplist_isempty (p : access constant pa_proplist) return int  -- /usr/include/pulse/proplist.h:410
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_isempty";

  --* Return non-zero when a and b have the same keys and values.
  -- * \since 0.9.16  

   function pa_proplist_equal (a : access constant pa_proplist; b : access constant pa_proplist) return int  -- /usr/include/pulse/proplist.h:414
   with Import => True, 
        Convention => C, 
        External_Name => "pa_proplist_equal";

end pulse_proplist_h;
