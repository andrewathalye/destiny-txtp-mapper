with Unchecked_Deallocation;

with vgmstream_h; use vgmstream_h;
with plugins_h; use plugins_h;

package VGMStream is
	-- Types
	type VGMStream_Access is access all vgmstream_h.VGMSTREAM; -- Not recommended to access components directly, since untranslated.
	subtype VGMStream_Config is vgmstream_cfg_t;
	type VGMStream_Config_Access is access VGMStream_Config;

	-- Constants
	VGMStream_CLI_Config : constant VGMStream_Config := (disable_config_override => 0,
		allow_play_forever => 0,
		play_forever => 0,
		fade_time => 10.0,
		loop_count => 2.0,
		fade_delay => 0.0,
		ignore_loop => 0,
		force_loop => 0,
		really_force_loop => 0,
		ignore_fade => 0);

	-- Subprograms
	-- Free VGMStream_Config_Acess
	procedure Free is new Unchecked_Deallocation 
		(Object => vgmstream_cfg_t, Name => VGMStream_Config_Access);
	
	-- Init VGMStream from path of input file
	function VGMStream_Init (S : String) return VGMStream_Access;

	-- Close VGMStream
	procedure VGMStream_Close (V : access vgmstream_h.VGMSTREAM) renames close_vgmstream;

	-- Read Number of Samples
	function VGMStream_Get_Samples (V : VGMStream_Access) return Natural;

	-- Read Sample Rate
	function VGMStream_Get_Sample_Rate (V : VGMStream_Access) return Natural is (Natural (V.all.sample_rate));

	-- Apply Configuration
	procedure VGMStream_Apply_Config (V : access vgmstream_h.VGMSTREAM; C : access vgmstream_cfg_t) renames plugins_h.vgmstream_apply_config;

end VGMStream;
