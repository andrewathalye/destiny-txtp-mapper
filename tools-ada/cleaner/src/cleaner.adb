with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Cleaner is
	type Text_Entry is record
		Size : Positive := 1;
		ID : String (1..14) := "0000-0000-0000";
	end record;

	Invalid_Entry_Error : Exception;

	T : Text_Entry; -- Current ID and Size
	N : Text_Entry; -- New ID and Size

	function Parse_Entry (I : in String) return Text_Entry  is
		S : String (1 .. 14);
		P : Positive;
	begin
		if I'Length < 17 then -- Minimum Length for valid ID and Size, since size length 1 cannot be handled
			raise Invalid_Entry_Error with "Invalid entry length";
		end if;
		S := I (I'First .. I'First + 13);	
		P := Integer'Value (I (I'First + 15 .. I'Last));
		return Text_Entry'(ID => S, Size => P);
	exception
		when Invalid_Entry_Error | Constraint_Error =>
			Put_Line (Standard_Error, "[Error] Failed to parse entry: " & I);
			raise;
	end Parse_Entry;

begin
	Put_Line (Standard_Error, Command_Name & ": clean sorted matches of duplicate banks"); 
	New_Line (Standard_Error);
	T := Parse_Entry (Get_Line);
	while not End_Of_File loop
		N := Parse_Entry (Get_Line);
		if T.ID /= N.ID then
			Put_Line (T.Size'Image & " " & T.ID);
			T.ID := N.ID;
			T.Size := N.Size;
		elsif T.Size < N.Size then
			T.Size := N.Size;
		end if;
	end loop;
	if T.ID = N.ID and T.Size > N.Size then
		Put_Line (T.Size'Image & " " & T.ID);
	else
		Put_Line (N.Size'Image & " " & N.ID);
	end if;
end Cleaner;
