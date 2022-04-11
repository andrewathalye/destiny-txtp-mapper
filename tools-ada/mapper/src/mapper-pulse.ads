package Mapper.Pulse is
	-- Task Types
	-- Play a track using PulseAudio and VGMStream
	-- F should be the relative path to a txtp file
	task type Play_Task is
		entry Play (F : String);	
	end Play_Task;

	-- Protected Objects
	protected Play_Task_Exit is
		procedure Set;
		procedure Unset;
		function Get return Boolean;
	private
		Local : Boolean := False;
	end Play_Task_Exit;
end Mapper.Pulse;
