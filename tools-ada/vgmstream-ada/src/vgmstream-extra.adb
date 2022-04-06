with Ada.Text_IO; use Ada.Text_IO;
with VGMStream; use VGMStream;

package body VGMStream.Extra is
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
end VGMStream.Extra;
