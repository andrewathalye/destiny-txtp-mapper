with Ada.Text_IO; use Ada.Text_IO;

package body Mapper.Confirm is
	-- Confirm input entry
	procedure Confirm_Entry (T : Text_Entry) is
		C : Character; -- Input character for response
	begin
		Put_Line ("[Info] Confirm " & T.Name.all & " (" & T.ID.all & ")");
		loop
			Play_Track (T);
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
					null;
			end case;
		end loop;
	end Confirm_Entry;
end Mapper.Confirm;
