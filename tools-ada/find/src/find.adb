with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;

procedure Find is
	-- Global Variables
	F : File_Type;

	procedure Process_Line (L : String) is
		I : constant String (1 .. 14) := (case L'Length is
			when 0 .. 14 => "",
			when others => L (L'First .. L'First + 13));

		Insufficient_Length : Exception;
	begin
		if I'Length = 0 then	
			raise Insufficient_Length with L;
		end if;

		while not End_Of_File (F) loop -- Read through tracks file to find ID
			declare		
				L : constant String := Get_Line (F);
				-- File format: X YYYY-YYYY-YYYY
				T : constant String (1 .. 14) := (case L'Length is
					when 0 .. 16 => "",
					when others => L (L'First + 2 .. L'First + 15));
			begin
				if T'Length = 0 then
					raise Insufficient_Length with L;
				end if;

				if I = T then -- If ID from stdin and tracks file are same, return
					return;
				end if;
			end;
		end loop;

		Put_Line (L); -- Output original line if not in tracks file
	exception
		when E : Insufficient_Length =>
			Put_Line (Standard_Error, "[Error] Malformed line: " & Exception_Message (E));
			return;
	end Process_Line;
begin
	Put_Line (Standard_Error, Command_Name & " tracks_all.txt: Find entries from stdin that are not contained in master track list.");

	if Argument_Count > 0 then
		Open (F, In_File, Argument (1));
	else
		Put_Line (Standard_Error, "[Error] Insufficient arguments provided.");
		Set_Exit_Status (Failure);
		return;
	end if;

	while not End_Of_File loop
		Process_Line (Get_Line);	
		-- Reset position in tracks file
		Reset (F);
	end loop;

	Close (F);
end Find;
