with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with VGMStream.Extra; use VGMStream.Extra;

package body Mapper.Identify is
	procedure Identify_Entry (T : Text_Entry) is
		-- File name
		F : constant String := "txtp/" & Swap_Whitespace (T.ID.all) & ".txtp";
		C : Character; -- Store y / n / ? responses
	begin
		Put_Line ("[Info] Identify " & T.Name.all & " (" & T.ID.all & ")");
		loop
			Put_Line ("[Info] Length:" & Integer'Image (Integer (Get_Length_Seconds (F))) & " seconds");
			Play_Track (F);
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
					begin
						Put_Line (Standard_Error, "! " & T.ID.all & " " & S);
					end;
					exit;
				when 'n' =>
					exit;
				when others =>
					null;
			end case;
		end loop;
	end Identify_Entry;
end Mapper.Identify;
