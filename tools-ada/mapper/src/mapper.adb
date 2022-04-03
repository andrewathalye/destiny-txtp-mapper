with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;

with Mapper.Text; use Mapper.Text;
with Mapper.Shared; use Mapper.Shared;
with Mapper.Export; use Mapper.Export;

package body Mapper is
	-- Exceptions
	Unimplemented_Error : Exception;

	-- Print usage message
	procedure Show_Usage is
	        U_1 : constant String := "Usage: " & Command_Name & " [options] [input] [outputdir]";
		U_2 : constant String := "-i: Identify mode. Entries marked with + will be played for you to identify. No output files will be produced.";
		U_3 : constant String := "-c: Confirm mode. Entries marked with ! will be played for you to identify. No output files will be produced.";
		U_4 : constant String := "-a: Approximate mode. Generate output for entries marked with !. Must not be paired with -i or -c.";
		U_5 : constant String := "-q: Quiet mode. Do not generate any warnings for unconfirmed or unidentified entries.";
	begin
		Put_Line (U_1);
		Put_Line (U_2);
		Put_Line (U_3);
		Put_Line (U_4);
		Put_Line (U_5);
	end Show_Usage;

	-- Main program body
	procedure Mapper is
		F : File_Type; -- Hold input file info
		T : Text_Entry; -- Current entry
		M : Shared.Mode := Default;
	begin
		Put_Line ("txtp renamer tool v0.6a");
		New_Line;
		Parse_Arguments (M, F);
		while not End_of_File (F) loop
			Fill_Entry (Get_Line (F), T);	
			case T.Entry_Type is	
				when Unidentified =>
					if M = Identify then
						raise Unimplemented_Error with "Identify";
					elsif not Quiet then
						Put_Line ("[Warn] Unidentified entry skipped: " & T.ID.all);
					end if;
				when Approximate =>
					if M = ApproximateConfirm then
						raise Unimplemented_Error with "Confirm";
					elsif M = ApproximateExport then
						Delegate_Export_Task (T);
					elsif not Quiet then
						Put_Line ("[Warn] Approximate entry skipped: " & T.ID.all);
					end if;
				when Confirmed =>
					if M = Default or M = ApproximateExport then
						Delegate_Export_Task (T);
					end if;
				when Comment =>
					null; -- No action needed
				when others =>
					null; -- Not logically possible (no action needed)
			end case;
			end loop;
		Close (F);
	exception
		when E : Entry_Error =>
			Put_Line (Standard_Error, "[Error] Unable to process entry:" & Exception_Message (E));
			Set_Exit_Status (Failure);
			return;
		when Arguments_Error =>
			Put_Line (Standard_Error, "[Error] Invalid argument combination or unknown argument.");
			Show_Usage;
			Set_Exit_Status (Failure);
			return;
	end Mapper;
end Mapper;
