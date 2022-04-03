with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

package body Mapper.Identify is
	procedure Identify_Entry (T : Text_Entry) is
		S : Search_Type;
		D : Directory_Entry_Type;
		C : Character; -- Store y / n / ? responses
	begin
		Put_Line ("[Info] Identify " & T.Name.all & " (" & T.ID.all & ")");
		-- Search for all txtp files in txtp/ beginning with T.ID
		Start_Search (S, "txtp", Swap_Whitespace (T.ID.all) & "*.txtp");
		while More_Entries (S) loop
			Get_Next_Entry (S, D);
			Put_Line ("[Info] Found " & Base_Name (Simple_Name (D)));
			loop
				Print_Length (Full_Name (D));
				Play_Track (Full_Name (D));
				Put ("[Interactive] Keep track? (y/n/?) ");
				Get (C);
				-- Clear buffer
				declare
					Discard : constant String := Get_Line;
				begin
					null;
				end;
				case C is
					when 'y' =>
						Put ("[Interactive] Enter name: ");
						declare
							S : constant String := Get_Line;
							N : constant String := Swap_Whitespace (Base_Name (Simple_Name (D)));
						begin
							Put_Line ("! " & N & " " & S);
						end;
						exit;
					when 'n' =>
						exit;
					when others =>
						null;
				end case;
			end loop;
		end loop;
		End_Search (S);
	end Identify_Entry;
end Mapper.Identify;
