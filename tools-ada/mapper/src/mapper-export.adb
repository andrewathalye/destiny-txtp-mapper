with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Mapper.Export is
	-- Local Exceptions
	Export_Exception : Exception;

	-- Local Variables
	H : Boolean := False; -- Has init export tasks

	-- Export specified entry. TODO: Use VGMStream API instead of separate executable
	procedure Export_Entry (T : Text_Entry; Task_ID : Positive) is
		ID : String renames T.ID.all;
		N : String renames T.Name.all;
		-- Input file construction
		I : constant String_Access := new String'("txtp/" & Swap_Whitespace (ID) & ".txtp");	
		-- Parameters construction
		P : constant String_Access := new String'("-o");
		-- Output file construction
		O : constant String_Access := new String'(Dir.all & "/" & N & ".wav");
		B : Boolean := False;
		A : constant Argument_List := (I, P, O);
	begin
		Put_Line ("[Info][T" & Task_ID'Image & "] Export " & N & " (" & ID & ")");
		Spawn (Program_Name => VGMStream_CLI_Path, Args => A, Success => B);	
		if not B then
			raise Export_Exception;
		end if;
	exception
		when Export_Exception =>
			Put_Line(Standard_Error, "[Error][T" & Task_ID'Image & "] Could not export track: " & N);
			return;
	end Export_Entry;

	-- Definition for status query of export task
	protected Export_Task_Busy is
		procedure Set (P : Positive);
		procedure Unset (P : Positive);
		function Query (P : Positive) return Boolean;
	private
		B : Boolean_Array (1 .. Max_Tasks) := (others => True);	-- Busy by default
	end Export_Task_Busy;

	-- Body for status query of export task
	protected body Export_Task_Busy is
		procedure Set (P : Positive) is
		begin
			B (P) := True;
		end Set;

		procedure Unset (P : Positive) is
		begin
			B (P) := False;
		end Unset;

		function Query (P : Positive) return Boolean is
		begin
			return B (P);
		end Query;
	end Export_Task_Busy;

	-- Tasks
	-- Parallel export task definition
	task type Export_Task is
		entry Init (P : Positive);
		entry Run (T : Text_Entry);
	end Export_Task;

	-- Parallel export task body
	task body Export_Task is
		Local_T : Text_Entry;
		Task_ID : Positive;
	begin
		loop
			select
				accept Init (P : Positive) do
					Task_ID := P;
					Export_Task_Busy.Unset (Task_Id);
				end Init;
			or
				accept Run (T : Text_Entry) do
					Local_T := T;
					Export_Task_Busy.Set (Task_ID);
				end Run;
			or
				terminate;
			end select;
			if Export_Task_Busy.Query (Task_ID) then
				Export_Entry (Local_T, Task_ID);
				Export_Task_Busy.Unset (Task_ID);
			end if;
		end loop;
	end Export_Task;

	-- Array of export tasks
	Export_Tasks : array (1 .. Max_Tasks) of Export_Task;

	-- Export Task utility subprograms
	procedure Init_Export_Tasks is
	begin
		for I in Export_Tasks'Range loop
			Export_Tasks (I).Init (I);	
		end loop;
		H := True; -- Tasks have been initialized
	end Init_Export_Tasks;

	-- Delegate a Text Entry to an export task
	procedure Delegate_Export_Task (T : in Text_Entry) is
	begin
		if not H then -- Tasks not yet initialized
			Init_Export_Tasks;
		end if;

		Outer_Loop:
		loop
			for I in Export_Tasks'Range loop
				if not Export_Task_Busy.Query (I) then
					Export_Tasks (I).Run (T);
					exit Outer_Loop;
				end if;
			end loop;
			delay 1.0; -- Delay 1 second before trying again
		end loop Outer_loop;
	end Delegate_Export_Task;

end Mapper.Export;
