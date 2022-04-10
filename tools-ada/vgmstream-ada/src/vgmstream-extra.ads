package VGMStream.Extra is
	Export_Exception : exception;

	function Get_Length (S : String) return Natural;
	function Get_Length_Seconds (S : String) return Float;
	procedure Export_Wav (O : String; I : String);
end VGMStream.Extra;
