package Mapper.Shared is
	-- Exceptions
	Handled_Fatal_Error : Exception;

	-- Constants
	Max_Tasks : constant Positive := 11;
	VGMStream_CLI_Path : constant String := "/home/andrew/bin/vgmstream-cli-silent";

	-- Shared Variables
	Dir : access String; -- Output directory
	Quiet : Boolean := False;

	-- Types
	type Mode is (Default, ApproximateExport, ApproximateConfirm, Identify);
	type Boolean_Array is array (Natural range <>) of Boolean;

	-- Subprograms
	function Swap_Whitespace (S : String) return String;
end Mapper.Shared;
