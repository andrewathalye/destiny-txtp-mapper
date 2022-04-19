package Mapper.Shared is
	-- Exceptions
	Handled_Fatal_Error : Exception;

	-- Constants
	Max_Tasks : constant Positive := 11;

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
end Mapper.Shared;
