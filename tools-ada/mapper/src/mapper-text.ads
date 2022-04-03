with Ada.Text_IO; use Ada.Text_IO;
with Mapper.Shared; use Mapper.Shared;

package Mapper.Text is
	-- Exceptions
	Entry_Error : Exception;
	Arguments_Error : Exception;

	-- Types
	type Text_Entry_Type is (Unidentified, Approximate, Confirmed, Comment, Other);
	type Text_Entry is record
		Entry_Type : Text_Entry_Type;
		ID : access String;
		Name : access String;
	end record;

	-- Subprograms
	procedure Fill_Entry (L : in String; T : out Text_Entry);
	procedure Parse_Arguments (M : in out Shared.Mode; F : out File_Type);

end Mapper.Text;
