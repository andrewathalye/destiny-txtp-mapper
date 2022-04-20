with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with VGMStream.Extra; use VGMStream.Extra;

package body Search_Task is
	-- Maximum Tasks
	Max_Tasks : constant Positive := 11;

	-- Task spec
	task type Search_Task is
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
		File_Name : Unbounded_String;
		Entry_ID : Unbounded_String;
	begin
		loop
			select
				accept Run (F : String; I : String) do
					File_Name := To_Unbounded_String (F);
					-- Make parsing the output easier
					Entry_Id := To_Unbounded_String (Swap_Whitespace (I));
				end Run;
			or
				terminate;
			end select;
			Put_Line (Natural'Image (Get_Length (To_String (File_Name))) & " " & To_String (Entry_ID));
		end loop;
	end Search_Task;

	Search_Tasks : array (1 .. Max_Tasks) of Search_Task;

	-- Delegate search to search task
	procedure Delegate_Search_Task (F : String; I : String) is
	begin
		Outer:
		loop
			for N of Search_Tasks loop
				select
					N.Run (F, I);
					exit Outer;
				else	
					null;
				end select;
			end loop;
			delay 0.005; -- Prevent loop from doing unnecessary idle waiting
		end loop Outer;
	end Delegate_Search_Task;
end Search_Task;
