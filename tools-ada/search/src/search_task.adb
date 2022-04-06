with Ada.Text_IO; use Ada.Text_IO;
with Unchecked_Deallocation;

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

	-- Task implementation
	task body Search_Task is
		-- String Access type
		type String_Access is access String;

		-- Free String_Access
		procedure Free is new Unchecked_Deallocation (Object => String, Name => String_Access);

		Task_ID : Positive;
		File_Name : String_Access;
		Entry_ID : String_Access;
	begin
		accept Init (P : Positive) do
			Task_ID := P;
			Put_Line ("Task " & Positive'Image (P) & " was init");
		end Init;
		loop
			select
				accept Run (F : String; I : String) do
					File_Name := new String'(F);
					Entry_Id := new String'(I);
					Search_Task_Busy.Set (Task_ID);
				end Run;
			or
				terminate;
			end select;
			Put_Line (Natural'Image (Get_Length (File_Name.all)) & " " & Entry_ID.all);
			Free (File_Name);
			Free (Entry_ID);
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
