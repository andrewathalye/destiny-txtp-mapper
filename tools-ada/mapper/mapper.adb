with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
--with VGMStream; use VGMStream;

procedure Mapper is
	-- Types
	type Mode is (Default, ApproximateExport, ApproximateConfirm, Identify);
	type Text_Entry_Type is (Unidentified, Approximate, Confirmed, Comment, Other);
	type Text_Entry is record
		Entry_Type : Text_Entry_Type;
		ID : access String;
		Name : access String;
	end record;
	type Boolean_Array is array (Natural range <>) of Boolean;

	-- Exceptions
	Unimplemented_Error : Exception;
	Entry_Error : Exception;
	Arguments_Error : Exception;
	Export_Exception : Exception;

	-- Constants
	Max_Tasks : constant Positive := 11;
	VGMStream_CLI_Path : constant String := "/home/andrew/bin/vgmstream-cli-silent";

	-- Global State Variables
	F : File_Type; -- Hold input file info
	F_O : Boolean := False; -- Is file set / open? Used by argument parser
	D : access String; -- Directory
	D_O : Boolean := False; -- Is directory set / open? Used by argument parser
	T : Text_Entry; -- Current entry being processed
	M : Mode := Default;
	Q : Boolean; -- Quiet

	-- Return new String with ' ' and '_' swapped from input string
	function Swap_Whitespace (S : in String) return String
	is
		O : String (S'Range);
	begin
		for I in S'Range loop
			O (I) := (case S (I) is
				when ' ' => '_',
				when '_' => ' ',
				when others => S (I));
		end loop;
		return O;
	end Swap_Whitespace;

	-- Export specified entry. TODO: Use VGMStream API instead of separate executable
	procedure Export_Entry (T : Text_Entry; Task_ID : Positive) is
		ID : String renames T.ID.all;
		N : String renames T.Name.all;
		-- Input file construction
		I : constant String_Access := new String'("txtp/" & Swap_Whitespace (ID) & ".txtp");	
		-- Parameters construction
		P : constant String_Access := new String'("-o");
		-- Output file construction
		O : constant String_Access := new String'(D.all & "/" & N & ".wav");
		B : Boolean := False;
		A : constant Argument_List := (I, P, O);
	begin
		Put_Line ("[Info][T" & Task_ID'Image & "] Export " & N & " (" & ID & ")");
--		Put_Line ("[Debug] vgmstream-cli" & " " & I.all & " " & P.all & " " & O.all);
		GNAT.OS_Lib.Spawn (VGMStream_CLI_Path, A, B);	
		if not B then
			raise Export_Exception;
		end if;
	exception
		when Export_Exception =>
			Put_Line(Standard_Error, "[Error][T" & Task_ID'Image & "] Could not export track: " & N);
			return; -- Graceful handling?
			-- raise Handled_Fatal_Error;
	end Export_Entry;

	-- Definition for status query of export task
	protected Export_Task_Busy is
		procedure Set (P : Positive);
		procedure Unset (P : Positive);
		function Query (P : Positive) return Boolean;
	private
		B : Boolean_Array (1 .. Max_Tasks) := (others => False);	
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
--					Put_Line ("[Debug][T" & P'Image & "] Init");
					Task_ID := P;
				end Init;
			or
				accept Run (T : Text_Entry) do
					Local_T := T;
					Export_Task_Busy.Set (Task_ID);
--					Put_Line ("[Debug][T" & Task_ID'Image & "] Run");
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
--			Put_Line ("[Debug] [T0]: Init export task" & I'Image);
			Export_Tasks (I).Init (I);	
		end loop;
	end Init_Export_Tasks;

	procedure Delegate_Export_Task is
	begin
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

	-- Given input line, fill in Text_Entry record
	procedure Fill_Entry (L : in String) is
	begin
--		Put_Line ("[Debug] " & L);
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
--		Put_Line ("[Debug] " & T.Entry_Type'Image);
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
--				Put_Line ("[Debug] N was equal to" & N'Image & " and L'Length is" & L'Length'Image);
				T.ID := new String'(L (L'First + 2 .. N - 1));
				T.Name := new String'(L (N + 1 .. L'Last));
			end;
		end if;
	end Fill_Entry;

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

	-- Parse command-line arguments
	procedure Parse_Arguments is
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
					Q := True;
				when '*' =>
					if not F_O then
						Open (F, In_File, Full_Switch);
						F_O := True;
					elsif not D_O then
						D := new String'(Full_Switch);
						D_O := True;
					else
						raise Arguments_Error;
					end if;
						
				when others =>
					exit;
			end case;
		end loop;
		if not F_O or not D_O then
			raise Arguments_Error;
		end if;
	exception
		when E : Name_Error =>
			Put_Line (Standard_Error, "[Error] Could not open file: " & Exception_Message (E));
			raise Handled_Fatal_Error;
	end Parse_Arguments;

begin -- Main program body
	Put_Line ("txtp renamer tool v0.6a");
	New_Line;
	Parse_Arguments;
	Init_Export_Tasks;
	while not End_of_File (F) loop
		Fill_Entry (Get_Line (F));	
		case T.Entry_Type is	
			when Unidentified =>
				if M = Identify then
					raise Unimplemented_Error with "Identify";
				elsif not Q then
					Put_Line ("[Warn] Unidentified entry skipped: " & T.ID.all);
				end if;
			when Approximate =>
				if M = ApproximateConfirm then
					raise Unimplemented_Error with "Confirm";
				elsif M = ApproximateExport then
					Delegate_Export_Task;
				elsif not Q then
					Put_Line ("[Warn] Approximate entry skipped: " & T.ID.all);
				end if;
			when Confirmed =>
				if M = Default or M = ApproximateExport then
					Delegate_Export_Task;
				end if;
			when Comment =>
				null; -- No action needed
			when others =>
				raise Entry_Error with T.Entry_Type'Image & ": unknown entry type.";
		end case;
	end loop;
	Close (F);
	-- No need to set F_O or D_O since program will terminate
exception
	when E : Entry_Error =>
		Put_Line (Standard_Error, "[Error] Unable to process entry:" & Exception_Message (E));
		Set_Exit_Status (Failure);
		return;
	when Arguments_Error | Invalid_Switch =>
		Put_Line (Standard_Error, "[Error] Invalid argument combination or unknown argument.");
		Show_Usage;
		Set_Exit_Status (Failure);
		return;
end Mapper;
