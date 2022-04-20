package Mapper.Pulse is
	-- Task Types
	-- Play a track using PulseAudio and VGMStream
	-- F should be the relative path to a txtp file
	task type Play_Task is
		entry Play (F : String);	
		entry Stop;
	end Play_Task;
end Mapper.Pulse;
