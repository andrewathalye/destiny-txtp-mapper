with util_h; use util_h;
with VGMStream; use VGMStream;

package VGMStream.Extra is
	Export_Exception : exception;

	function Get_Length (S : String) return Natural;
	function Get_Length_Seconds (S : String) return Float;
	procedure Export_Wav (O : String; I : String);

	-- Swap samples so that output is Little Endian
	procedure Swap_Samples_LE (A : Sample_Buffer_Access; C : int) renames util_h.swap_samples_le;
end VGMStream.Extra;
