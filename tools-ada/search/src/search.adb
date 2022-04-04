with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Unchecked_Deallocation;

with vgmstream_h; use vgmstream_h;
with plugins_h; use plugins_h;

procedure Search is
	S : Search_Type;
	N : Natural := 0; -- Duration
	D : Directory_Entry_Type;

	-- Free vgmstream_cfg_access
	type vgmstream_cfg_access is access vgmstream_cfg_t;
	procedure Free is new Unchecked_Deallocation
		(Object => vgmstream_cfg_t, Name => vgmstream_cfg_access);

	-- Get duration of file given path (VGMStream)
	function Get_Length (S : String) return Natural is
		C : chars_ptr := New_String (S);
		V : constant access VGMSTREAM := init_vgmstream (C);
		I : int;
		-- Default settings from vgmstream-cli
		VCFG : constant vgmstream_cfg_t := (disable_config_override => 0,
			allow_play_forever => 0,
			play_forever => 0,
			fade_time => 10.0,
			loop_count => 2.0,
			fade_delay => 0.0,
			ignore_loop => 0,
			force_loop => 0,
			really_force_loop => 0,
			ignore_fade => 0);
		VCFG_A : vgmstream_cfg_access := new vgmstream_cfg_t'(VCFG);
	begin
		if V /= null then
			vgmstream_apply_config (V, VCFG_A);
			I := V.all.pstate.play_duration;
			close_vgmstream (V);
			Free (VCFG_A);
		else
			Put_Line (Standard_Error, "[Error] Failed to process file: " & S);
			I := 0;
		end if;
		Free (C);
		return Natural (I); -- Cannot be less than zero per C definition
	end Get_Length;
begin
	Start_Search (S, "txtp/", "*.txtp");	
	while More_Entries (S) loop
		Get_Next_Entry (S, D);	
		declare
			T : constant String := Simple_Name (D);
			I : String (1 .. 14); -- ID
		begin
			if T'Length > 14 then
				I := T (T'First .. T'First + 13);	
				Put_Line (Natural'Image (Get_Length (Full_Name (D))) & " " & I);
			else
				Put_Line (Standard_Error, "[Error] Invalid file name length: " & T);
			end if;
		end;
	end loop;
	End_Search (S);
end Search;
