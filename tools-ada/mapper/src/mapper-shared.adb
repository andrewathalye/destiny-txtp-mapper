with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;

package body Mapper.Shared is
	-- Return new String with ' ' and '_' swapped from input string
	function Swap_Whitespace (S : in String) return String
	is
		O : String (S'Range);
	begin
		for I in S'Range loop
			O (I) := (case S (I) is
				when ' ' => '_',
				when '_' => ' ',
				when others => S (I));
		end loop;
		return O;
	end Swap_Whitespace;

	-- Play a track by Text_Entry
	procedure Play_Track (T : Text_Entry) is
	begin
		Play_Track ("txtp/" & Swap_Whitespace (T.ID.all) & ".txtp");
	end Play_Track;

	-- Play a track using vgmstream123 TODO: Use the API
	-- S should be the relative path to a txtp file
	procedure Play_Track (S : String) is
		P : String_Access := new String'(S); -- Filename parameter
		A : constant Argument_List := (1 => P);
		B : Boolean := False; -- Result of process
		Play_Error : Exception;
	begin
		Spawn (VGMStream123_Path, A, B);
		Free (P);
		if not B then
			raise Play_Error;
		end if;
	exception
		when Play_Error =>
			Put_Line (Standard_Error, "[Error] Could not play track: " & S);
			raise Handled_Fatal_Error;
	end Play_Track;

	-- Print duration of txtp from file path
	procedure Print_Length (S : String) is
		P : String_Access := new String'(S); -- Filename parameter
		A : constant Argument_List := (1 => P);
		B : Boolean := False; -- Process result
		Print_Length_Error : Exception;
	begin
		Spawn (VGMStream_Length_Path, A, B);
		Free (P);
		if not B then
			raise Print_Length_Error;
		end if;
	exception
		when Print_Length_Error => -- Non-fatal error
			Put_Line (Standard_Error, "[Error] Could not print length of track: " & S);
			return;
	end Print_Length;
end Mapper.Shared;
