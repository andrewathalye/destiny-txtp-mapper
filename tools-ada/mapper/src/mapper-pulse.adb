with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

with pulse_simple_h; use pulse_simple_h;
with pulse_def_h; use pulse_def_h;
with pulse_sample_h; use pulse_sample_h;

with VGMStream; use VGMStream;
with VGMStream_Common; use VGMStream_Common;
with VGMStream.Extra; use VGMStream.Extra;

with Mapper.Shared; use Mapper.Shared;

package body Mapper.Pulse is

	-- Constants for task (avoid duplication)
	Config : constant VGMStream_Config_Access := new VGMStream_Config'(VGMStream_CLI_Config); -- Only needed once, so will not be freed
	App_Name : constant chars_ptr := New_String ("txtp-mapper"); -- Only needed once, so will not be freed
	SBS : constant Natural := 32768;

	protected body Play_Task_Exit is	
		procedure Set is
		begin
			Local := True;
		end Set;

		procedure Unset is
		begin
			Local := False;
		end Unset;

		function Get return Boolean is (Local);
	end Play_Task_Exit;	

	task body Play_Task is
		V : VGMStream_Access;

		type PSS_Access is access pa_sample_spec;
		S : PSS_Access;

		File_Name : chars_ptr;
		P : access pa_simple;

		Play_Error : Exception;
	begin
		loop
			select
				accept Play (F : String) do
					V := VGMStream_Init (F);
					File_Name := New_String (F);
					if V = null then
						raise Play_Error with "VGMStream instance was null";
					end if;
		
					-- Connect to PulseAudio
					S := new pa_sample_spec'(PA_SAMPLE_S16LE, unsigned (V.all.sample_rate), unsigned_char (V.all.channels));
					P := pa_simple_new (To_Chars_Ptr (null), App_Name, PA_STREAM_PLAYBACK, To_Chars_Ptr (null), File_Name, S, null, null, null);
					Free (File_Name);
					if P = null then
						raise Play_Error with "PulseAudio instance was null";
					end if;
		
					VGMStream_Apply_Config (V, Config);
					Play_Task_Exit.Unset; -- Make sure will not exit early if reused.
				end Play;
		
				declare
					L : constant Natural := VGMStream_Get_Samples (V);
					B : Sample_Buffer_Access := new Sample_Buffer (1 .. L);
					I : Natural := 0;
					T : Natural := 0;

					-- Cycle is 1/5 less than the time that a sample should last for. This allows for I/O and other margin of error
					-- to reduce the number of audible gaps. Gaps may still occur if PulseAudio is used instead of PipeWire or real-time
					-- scheduling is not used.
					Cycle : constant Time_Span := Milliseconds ( Natural (Float (SBS) / Float (V.all.sample_rate) * Float (1000)) - 100);
					Desired_Time : Time := Clock + Cycle;	
		
					pragma Warnings (Off); -- This pointer will have bounds by definition above, but does not in the generic case
					package SATAC is new System.Address_To_Access_Conversions (Object => Sample_Buffer);
					pragma Warnings (On);
				begin
					loop
						-- Prevent asking for too many samples
						T := SBS;
						if I + SBS > L then
							T := L - I;
						end if;

						-- Render audio to buffer
						if VGMStream_Render (B, int (T), V) /= int (SBS) then
							raise Play_Error with "Failed to render audio data to buffer";
						end if;
						Swap_Samples_LE (B, V.all.channels * int (T));

						-- Bytes to write is Samples (T) * Bytes Per Sample * Channels
						if pa_simple_write (P, SATAC.To_Address (SATAC.Object_Pointer (B)), unsigned_long (T * Sample'Size / 8 * Integer (V.all.channels)), null) /= 0 then
							raise Play_Error with "Failed to send data to PulseAudio";
						end if;

						delay until Desired_Time; -- Minimise stress on CPU / PulseAudio
						Desired_Time := Desired_Time + Cycle;
						if I >= L or Play_Task_Exit.Get then -- Exit if we've reached the end or been asked to exit by another thread
							exit;
						end if;

						I := I + SBS;
					end loop;
					Free (B);
				end;
				pa_simple_free (P);
				VGMStream_Close (V);
			or
				terminate;
			end select;
		end loop;
	exception
		when Play_Error =>
			Put_Line (Standard_Error, "[Error] Could not play track: " & Value (File_Name));
			raise Handled_Fatal_Error;
	end Play_Task;
end Mapper.Pulse;
