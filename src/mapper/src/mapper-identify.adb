with Ada.Text_IO; use Ada.Text_IO;

with VGMStream.Extra; use VGMStream.Extra;
with Mapper.Pulse; use Mapper.Pulse;

package body Mapper.Identify is
	procedure Identify_Entry (T : Text_Entry) is
		-- File name
		F : constant String := "txtp/" & Swap_Whitespace (T.ID.all) & ".txtp";
		C : Character; -- Store y / n / ? responses
		PT : Play_Task;
	begin
		Put_Line ("[Info] Identify " & T.Name.all & " (" & T.ID.all & ")");
		Put_Line ("[Info] Length:" & Integer'Image (Integer (Get_Length_Seconds (F))) & " seconds");
		loop
			PT.Play (F);
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
					PT.Stop;
			end case;
		end loop;

		PT.Stop;
	end Identify_Entry;
end Mapper.Identify;
