with Ada.Text_IO; use Ada.Text_IO;

with Mapper.Pulse; use Mapper.Pulse;

package body Mapper.Confirm is
	-- Confirm input entry
	procedure Confirm_Entry (T : Text_Entry) is
		C : Character; -- Input character for response
		PT : Play_Task;
	begin
		Put_Line ("[Info] Confirm " & T.Name.all & " (" & T.ID.all & ")");
		loop
			PT.Play ("txtp/" & Swap_Whitespace (T.ID.all) & ".txtp");
			Put ("[Interactive] Keep current name (y/n/?) ");
			Get (C);
			declare -- Clear out residual newline
				Discard : constant String := Get_Line;
			begin
				null;
			end;

			case C is
				when 'y' =>
					Put_Line (Standard_Error, ". " & T.ID.all & " " & T.Name.all);
					exit;
				when 'n' =>
					Put ("[Interactive] Enter new name: ");
					declare
						S : constant String := Get_Line;
					begin
						Put_Line (Standard_Error, ". " & T.ID.all & " " & S);
						exit;
					end;
				when others =>
					Play_Task_Exit.Set;
			end case;
		end loop;

		Play_Task_Exit.Set; -- Exit task if still going
	end Confirm_Entry;
end Mapper.Confirm;
