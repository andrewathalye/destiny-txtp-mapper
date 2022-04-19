package body Mapper.Shared is
	-- Return new String with ' ' and '_' swapped from input string
	function Swap_Whitespace (S : in String) return String
	is
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
end Mapper.Shared;
