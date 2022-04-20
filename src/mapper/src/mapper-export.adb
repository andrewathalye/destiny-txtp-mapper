with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with VGMStream.Extra; use VGMStream.Extra;
package body Mapper.Export is
	-- Local Types
	subtype Task_Range is Positive range 1 .. Max_Tasks;

	-- Export specified entry.
	procedure Export_Entry (T : Text_Entry) is
		I : String renames T.ID.all;
		N : String renames T.Name.all;
	begin
		Put_Line ("[Info] Export " & N & " (" & I & ")");
		Export_Wav (Dir.all & "/" & N & ".wav", "txtp/" & Swap_Whitespace (I) & ".txtp");
	exception
		when E : Export_Exception =>
			Put_Line(Standard_Error, "[Error] Could not export track: " & N & ": " & Exception_Message (E));
			return;
	end Export_Entry;

	-- Tasks
	-- Parallel export task definition
	task type Export_Task is
		entry Run (T : Text_Entry);
	end Export_Task;

	-- Parallel export task body
	task body Export_Task is
		Local_T : Text_Entry;
	begin
		loop
			select
				accept Run (T : Text_Entry) do
					Local_T := T;
				end Run;
			or
				terminate;
			end select;

			Export_Entry (Local_T);
		end loop;
	end Export_Task;

	-- Array of export tasks
	Export_Tasks : array (Task_Range) of Export_Task;

	-- Delegate a Text Entry to an export task
	procedure Delegate_Export_Task (T : in Text_Entry) is
	begin
		Outer_Loop:
		loop
			for I of Export_Tasks loop
				select
					I.Run (T);
					exit Outer_Loop;
				else -- If not immediately available to run, try next
					null;
				end select;
			end loop;
			delay 0.5; -- Delay 0.5 second before trying again
		end loop Outer_loop;
	end Delegate_Export_Task;

end Mapper.Export;
