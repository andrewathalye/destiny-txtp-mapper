with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;

with Search_Task; use Search_Task;

procedure Search is
	S : Search_Type;
	D : Directory_Entry_Type;

begin
	Put_Line (Standard_Error, Command_Name & ": Search txtp directory for files and output with length");
	Init_Search_Tasks; -- Setup tasks
	Start_Search (S, "txtp/", "*.txtp");	
	while More_Entries (S) loop
		Get_Next_Entry (S, D);	
		declare
			T : constant String := Simple_Name (D);
			I : String (1 .. 14); -- ID
		begin
			if T'Length > 14 then
				I := T (T'First .. T'First + 13);	
				Delegate_Search_Task (Full_Name (D), I); -- Send work to task
			else
				Put_Line (Standard_Error, "[Error] Invalid file name length: " & T);
			end if;
		end;
	end loop;
	End_Search (S);
end Search;
