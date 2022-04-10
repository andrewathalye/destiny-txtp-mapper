package Mapper.Shared is
	-- Exceptions
	Handled_Fatal_Error : Exception;

	-- Constants
	Max_Tasks : constant Positive := 11;
	VGMStream123_Path : constant String := "./tools-ext/vgmstream123-silent";

	-- Shared Variables
	Dir : access String; -- Output directory
	Quiet : Boolean := False;

	-- Types
	type Mode is (Default, ApproximateExport, ApproximateConfirm, Identify);
	type Boolean_Array is array (Natural range <>) of Boolean;

	type Text_Entry_Type is (Unidentified, Approximate, Confirmed, Comment, Other);
	type ID_Access is access String;
	type Name_Access is access String;
	type Text_Entry is record
		Entry_Type : Text_Entry_Type;
		ID : ID_Access;
		Name : Name_Access;
	end record;

	-- Subprograms
	function Swap_Whitespace (S : String) return String; -- Replace ' ' with '_' and vice-versa
	procedure Play_Track (T : Text_Entry); -- Play track from Text_Entry
	procedure Play_Track (S : String); -- Play track from file path
end Mapper.Shared;
