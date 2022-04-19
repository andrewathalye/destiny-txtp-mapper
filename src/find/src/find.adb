with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;

procedure Find is
	-- Global Variables
	F : File_Type;

	procedure Process_Line (L : String) is
		Insufficient_Length : Exception;
		Offset : Natural := 0;
	begin
		-- Line format: Size ID
		-- Find offset of ID
		for N in L'Range loop
			if L (N) = ' ' then
				Offset := N + 1;
				exit;
			end if;
		end loop;

		-- Ensure that substring range will be valid
		if Offset = 0 or Offset >= L'Length - 1 then
			raise Insufficient_Length with L;
		end if;

		declare
			I : constant String := L (Offset .. L'Last);
		begin
			while not End_Of_File (F) loop -- Read through tracks file to find ID
				declare		
					L : constant String := Get_Line (F);
					First_Offset : Natural := 0;
					Second_Offset : Natural := 0;
				begin
					-- Line format: X ID Name
					-- Find offset of ID
					for N in L'Range loop
						if L (N) = ' ' then
							if First_Offset = 0 then
								First_Offset := N + 1;
							else
								Second_Offset := N - 1;
								exit;
							end if;
						end if;
					end loop;

					-- Ensure that substring range will be valid
					-- Skip blank lines
					if L'Length /= 0 then
						if Second_Offset = 0 or Second_Offset >= L'Length then
							raise Insufficient_Length with L;
						end if;

						declare
							T : constant String := L (First_Offset .. Second_Offset);
						begin
							if I = T then -- If ID from stdin and tracks file are same, return
								return;
							end if;
						end;
					end if;
				end;
			end loop;
		end;

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
