with Ada.Text_IO; use Ada.Text_IO;
with Mapper.Shared; use Mapper.Shared;

package Mapper.Text is
	-- Exceptions
	Arguments_Error : Exception;

	-- Subprograms
	procedure Fill_Entry (L : in String; T : out Text_Entry);
	procedure Parse_Arguments (M : in out Shared.Mode; F : out File_Type);

end Mapper.Text;
