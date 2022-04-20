with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Pulse; use Pulse;

with VGMStream; use VGMStream;
with VGMStream.Extra; use VGMStream.Extra;

with Mapper.Shared; use Mapper.Shared;

package body Mapper.Pulse is

	-- Constants for task (avoid duplication)
	App_Name : constant chars_ptr := New_String ("txtp-mapper"); -- Only needed once, so will not be freed

	task body Play_Task is
		V : VGMStream_Access;

		S : Pulse_Sample_Spec_Access;

		File_Name : chars_ptr;
		P : Pulse_Simple_Access;

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
					S := new Pulse_Sample_Spec'(PA_SAMPLE_S16LE, Unsigned_32 (V.all.sample_rate), Unsigned_8 (V.all.channels));
					P := Pulse_Simple_New (To_Chars_Ptr (null), App_Name, PA_STREAM_PLAYBACK, To_Chars_Ptr (null), File_Name, S, null, null, null);
					Free (File_Name);
					if P = null then
						raise Play_Error with "PulseAudio instance was null";
					end if;
		
					VGMStream_Apply_Config (V, VGMStream_CLI_Config'Access);
				end Play;

				declare
					L : constant Natural := VGMStream_Get_Samples (V);
					I : Natural := 0;
					T : Natural := 0;

					-- Buffer to hold samples
					B : aliased Sample_Buffer (1 .. Sample_Buffer_Size * Natural (V.all.channels));

					-- Cycle is 1/5 less than the time that a sample should last for. This allows for I/O and other margin of error
					-- to reduce the number of audible gaps. Gaps may still occur if PulseAudio is used instead of PipeWire or real-time
					-- scheduling is not used.
					Cycle : constant Time_Span := Milliseconds ( Natural (Float (Sample_Buffer_Size) / Float (V.all.sample_rate) * Float (1000)) - 100);
					Desired_Time : Time := Clock + Cycle;	
				begin
					while I < L loop
						-- Prevent asking for too many samples
						T := Sample_Buffer_Size;
						if I + Sample_Buffer_Size > L then
							T := L - I;
						end if;

						-- Render audio to buffer
						if VGMStream_Render (B'Address, int (T), V) /= int (Sample_Buffer_Size) then
							raise Play_Error with "Failed to render audio data to buffer";
						end if;
						Swap_Samples_LE (B'Address, V.all.channels * int (T));

						-- Bytes to write is Samples (T) * Bytes Per Sample * Channels
						if Pulse_Simple_Write (P, B'Address, size_t (T * Sample_Type'Size / 8 * Integer (V.all.channels)), null) /= 0 then
							raise Play_Error with "Failed to send data to PulseAudio";
						end if;

						delay until Desired_Time; -- Minimise stress on CPU / PulseAudio
						Desired_Time := Desired_Time + Cycle;

						-- If Stop entry has been called, exit loop, otherwise keep going
						select
							accept Stop;
							exit;
						else
							null;
						end select;

						I := I + Sample_Buffer_Size;
					end loop;
				end;
				Pulse_Simple_Free (P);
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
