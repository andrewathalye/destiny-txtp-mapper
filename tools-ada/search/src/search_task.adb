with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with VGMStream.Extra; use VGMStream.Extra;

package body Search_Task is
	-- Maximum Tasks
	Max_Tasks : constant Positive := 11;

	-- Query state of Search_Task
	type Search_Task_Busy_Array is array (1 .. Max_Tasks) of Boolean;
	
	-- Shared protected spec
	protected Search_Task_Busy is
		procedure Set (P : Positive);
		procedure Unset (P : Positive);
		function Query (P : Positive) return Boolean;
		private
			B : Search_Task_Busy_Array := (others => False);
	end Search_Task_Busy;

	-- Shared protected implementation
	protected body Search_Task_Busy is
		procedure Set (P : Positive) is	
		begin
			B (P) := True;
		end Set;

		procedure Unset (P : Positive) is
		begin
			B (P) := False;
		end Unset;

		function Query (P : Positive) return Boolean is (B (P));	
	end Search_Task_Busy;

	-- Task spec
	task type Search_Task is
		entry Init (P : Positive);
		entry Run (F : String; I : String);
	end Search_Task;

	-- Replace '_' with ' ' and vice-versa
	function Swap_Whitespace (S : in String) return String is	
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

	-- Task implementation
	task body Search_Task is
		Task_ID : Positive;
		File_Name : Unbounded_String;
		Entry_ID : Unbounded_String;
	begin
		accept Init (P : Positive) do
			Task_ID := P;
		end Init;
		loop
			select
				accept Run (F : String; I : String) do
					File_Name := To_Unbounded_String (F);
					-- Make parsing the output easier
					Entry_Id := To_Unbounded_String (Swap_Whitespace (I));
					Search_Task_Busy.Set (Task_ID);
				end Run;
			or
				terminate;
			end select;
			Put_Line (Natural'Image (Get_Length (To_String (File_Name))) & " " & To_String (Entry_ID));
			Search_Task_Busy.Unset (Task_ID);
		end loop;
	end Search_Task;

	Search_Tasks : array (1 .. Max_Tasks) of Search_Task;

	-- Delegate search to search task
	procedure Delegate_Search_Task (F : String; I : String) is
	begin
		Outer:
		loop
			for N in Search_Tasks'Range loop
				if not Search_Task_Busy.Query (N) then
					Search_Tasks (N).Run (F, I);	
					exit Outer;
				end if;
			end loop;
			delay 0.005; -- Prevent loop from doing unnecessary idle waiting
		end loop Outer;
	end Delegate_Search_Task;

	-- Initialise tasks
	procedure Init_Search_Tasks is
	begin
		for I in Search_Tasks'Range loop
			Search_Tasks (I).Init (I);
		end loop;
	end Init_Search_Tasks;
end Search_Task;
