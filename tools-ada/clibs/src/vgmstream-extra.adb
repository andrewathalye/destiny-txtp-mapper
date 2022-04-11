with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with Ada.Directories; use Ada.Directories;

with VGMStream; use VGMStream;

package body VGMStream.Extra is
	-- WAV Header type
	type Wav_String is new String (1 .. 4);
	type Wav_Header is record
		RIFF : Wav_String := "RIFF";
		RIFF_Size : Unsigned_32;
		WAVE : Wav_String := "WAVE";
		WAVE_fmt : Wav_String := "fmt ";
		WAVE_fmt_Size : Unsigned_32 := 16#10#;
		Codec : Unsigned_16 := 1; -- PCM
		Channel_Count : Unsigned_16;
		Sample_Rate : Unsigned_32;
		Bytes_Per_Second : Unsigned_32;
		Block_Align : Unsigned_16;
		Significant_Bits_Per_Sample : Unsigned_16;
		WAVE_data : Wav_String := "data";
		WAVE_data_Size : Unsigned_32;
	end record;

	-- Get duration of file given path (VGMStream)
	function Get_Length (S : String) return Natural is
		V : constant VGMStream_Access := VGMStream_Init (S);
		I : Natural;
		-- Default settings from vgmstream-cli
		C : VGMStream_Config_Access  := new VGMStream_Config'(VGMStream_CLI_Config);
	begin
		if V /= null then
			VGMStream_Apply_Config (V, C); -- Set fade time, etc. so samples match vgmstream-cli
			I := VGMStream_Get_Samples (V);
			VGMStream_Close (V);
		else
			Put_Line (Standard_Error, "[Error] Failed to process file: " & S);
			I := 0;
		end if;
		Free (C); -- Free VGMStream_Config_Access
		return I;
	end Get_Length;

	-- Get duration in seconds of file given path
	function Get_Length_Seconds (S : String) return Float is
		V : constant VGMStream_Access := VGMStream_Init (S);
		F : Float;
		-- Default settings from vgmstream-cli
		C : VGMStream_Config_Access := new VGMStream_Config'(VGMStream_CLI_Config);
	begin
		if V /= null then
			VGMStream_Apply_Config (V, C);
			F := (Float (VGMStream.VGMStream_Get_Samples (V))) / (Float (VGMStream_Get_Sample_Rate (V)));
			VGMStream_Close (V);
		else
			Put_Line (Standard_Error, "[Error] Failed to process file: " & S);
			F := 0.0;
		end if;
		Free (C);
		return F;
	end Get_Length_Seconds;

	-- Create and write WAV header given VGMStream instance and Stream
	function Wav_Header_Create (V : VGMStream_Access) return Wav_Header is
		-- Data size
		D : constant Unsigned_32 := Unsigned_32 (VGMStream_Get_Samples (V) * V.all.channels) * (Sample'Size / 8);
	begin
		return Wav_Header'(RIFF_Size => 16#2c# - 16#8# + D,
			Channel_Count => Unsigned_16 (V.all.channels),
			Sample_Rate => Unsigned_32 (V.all.sample_rate),
			Bytes_Per_Second => Unsigned_32 (V.all.sample_rate * V.all.channels * (Sample'Size / 8)),
			Block_Align => Unsigned_16 (V.all.channels * (Sample'Size / 8)),
			Significant_Bits_Per_Sample => Unsigned_16 (Sample'Size),
			WAVE_data_Size => D,
			others => <>);
			
	end Wav_Header_Create;

	-- Export VGMStream input file to WAV file
	procedure Export_Wav (O : String; I : String) is
		V : VGMStream_Access := VGMStream_Init (I);
		C : VGMStream_Config_Access := new VGMStream_Config'(VGMStream_CLI_Config);
		SBS : constant Natural := 32768; -- Sample Buffer Size
		F : Ada.Streams.Stream_IO.File_Type;
		S : Stream_Access;
	begin	
		if V = null then
			raise Export_Exception with "Could not create VGMStream";
		end if;

		if Exists (O) then
			raise Export_Exception with "File exists";
		end if;
		Create (F, Out_File, O);
		S := Stream (F);

		-- Apply config so that export length will be correct
		VGMStream_Apply_Config (V, C);
		Wav_Header'Write (S, Wav_Header_Create (V));

		declare
			L : constant Natural := VGMStream_Get_Samples (V); -- Samples Length
			B : Sample_Buffer_Access := new Sample_Buffer (1 .. SBS * Natural (V.all.channels));
			I : Natural := 0;
			T : Natural; -- To Get
		begin
			loop
				T := (if I + SBS > L then L - I else SBS); -- Target samples export
				pragma Warnings (Off, "-gnatwx"); -- C-style pointer issues
				if VGMStream_Render (B, int (T), V) /= int (T) then
					Put_Line (Standard_Error, "[Warning] Less samples than expected returned");
				end if;
				Swap_Samples_LE (B, V.all.channels * int (T));
				pragma Warnings (On);
				Sample_Buffer'Write (S, B.all);	
				I := I + SBS;
				if I >= L then
					exit;
				end if;
			end loop;
			Free (B);
		end;
	
		Close (F);
		VGMStream_Close (V);
		Free (C);
	end Export_Wav;
	
end VGMStream.Extra;
