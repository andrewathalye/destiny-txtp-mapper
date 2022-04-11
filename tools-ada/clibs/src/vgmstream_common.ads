with streamtypes_h; use streamtypes_h;
with Unchecked_Deallocation;

package VGMStream_Common is
	subtype Sample is sample_t;
	type Sample_Buffer is array (Natural range <>) of Sample;

	type Sample_Buffer_Access is access all Sample_Buffer;

	procedure Free is new Unchecked_Deallocation (Object => Sample_Buffer, Name => Sample_Buffer_Access);
end VGMStream_Common;
