with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;

package body Mapper.Text is
	-- Private-Use Booleans
	F_O : Boolean := False; -- Is file open
	Dir_O : Boolean := False; -- Is directory set

	-- Procedures
	-- Given input line, fill in Text_Entry record
	procedure Fill_Entry (L : in String; T : out Text_Entry) is
		Entry_Error : Exception;
	begin
		if L'Length = 0 then
			T.Entry_type := Comment;
		else	
			T.Entry_Type := (case L (L'First) is 
				when '!' => Approximate,
				when '+' => Unidentified,
				when '.' => Confirmed,
				when '#' => Comment,
				when others => Other);
		end if;
		-- Raise exception if any unknown entries found
		if T.Entry_Type = Other then
			raise Entry_Error with L & ": unknown entry type.";
		end if;
		-- Comment entries might be malformed
		if T.Entry_Type /= Comment then
			declare
				N : Natural := L'First + 2; -- ID start
			begin
				while L (N) /= ' ' and N < L'Last loop
					N := N + 1;
				end loop;
				T.ID := new String'(L (L'First + 2 .. N - 1));
				T.Name := new String'(L (N + 1 .. L'Last));
			end;
		end if;
	exception
		when E : Entry_Error =>
			Put_Line (Standard_Error, "[Error] Unable to process entry:" & Exception_Message (E));
			Set_Exit_Status (Failure);
			raise Handled_Fatal_Error;
	end Fill_Entry;

	-- Parse command-line arguments
	procedure Parse_Arguments (M : in out Shared.Mode; F : out File_Type) is
	begin
		if Argument_Count = 0 then
			raise Arguments_Error;
		end if;
		loop
			case Getopt("* i c a q") is
				when 'i' =>
					if M = Default then
						M := Identify;
					else
						raise Arguments_Error;
					end if;
				when 'c' =>
					if M = Default then
						M := ApproximateConfirm;
					else
						raise Arguments_Error;
					end if;
				when 'a' =>
					if M = Default then
						M := ApproximateExport;
					else
						raise Arguments_Error;
					end if;
				when 'q' =>
					Quiet := True;
				when '*' =>
					if not F_O then
						Open (F, In_File, Full_Switch);
						F_O := True;
					elsif not Dir_O then
						Dir := new String'(Full_Switch);
						Dir_O := True;
					else
						raise Arguments_Error;
					end if;
						
				when others =>
					exit;
			end case;
		end loop;
		if not F_O or not Dir_O then
			raise Arguments_Error;
		end if;
	exception
		when E : Name_Error =>
			Put_Line (Standard_Error, "[Error] Could not open file: " & Exception_Message (E));
			raise Handled_Fatal_Error;
		when Invalid_Switch =>
			raise Arguments_Error;
	end Parse_Arguments;

end Mapper.Text;
