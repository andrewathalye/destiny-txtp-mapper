with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

package Pulse is
	-- Pulse Types
	type Pulse_Stream_Direction_Type is 
	     (PA_STREAM_NODIRECTION,
	      PA_STREAM_PLAYBACK,
	      PA_STREAM_RECORD,
	      PA_STREAM_UPLOAD)
	   with Convention => C;

	
	subtype Pulse_Sample_Format is int;
   	PA_SAMPLE_S16LE : constant Pulse_Sample_Format := 3;

	type Pulse_Sample_Spec is record
	      format : aliased Pulse_Sample_Format;
	      rate : aliased Unsigned_32;
	      channels : aliased Unsigned_8;
	end record;

	type Pulse_Sample_Spec_Access is access Pulse_Sample_Spec;

	-- Opaque Types
	type Pulse_Simple is null record;
	type Pulse_Simple_Access is access Pulse_Simple;
	type Pulse_Channel_Map is null record;
	type Pulse_Buffer_Attr is null record;

	-- Subprograms
	procedure Pulse_Simple_Free (P : Pulse_Simple_Access)
	with
		Import => True,
		Convention => C,
		External_Name => "pa_simple_free";
	
	function Pulse_Simple_Write (P : Pulse_Simple_Access; B : System.Address; S : size_t; E : access int) return int
	with
		Import => True,
		Convention => C,
		External_Name => "pa_simple_write";
	
	function Pulse_Simple_New (Server : chars_ptr; Name : chars_ptr; PSD : Pulse_Stream_Direction_Type; Dev : chars_ptr; Stream_Name : chars_ptr; SS : access constant Pulse_Sample_Spec; Map : access constant Pulse_Channel_Map; Attr : access constant Pulse_Buffer_Attr; Error : access int) return Pulse_Simple_Access
	with
		Import => True,
		Convention => C,
		External_Name => "pa_simple_new";
end Pulse;
