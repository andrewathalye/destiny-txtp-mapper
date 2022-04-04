pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_stdint_intn_h;

package streamtypes_h is

  -- * streamtypes.h - widely used type definitions
  --  

  -- Common versions:
  -- * - 1500: VS2008
  -- * - 1600: VS2010
  -- * - 1700: VS2012
  -- * - 1800: VS2013
  -- * - 1900: VS2015
  -- * - 1920: VS2019  

  --TODO: deprecated, remove
   subtype sample is bits_stdint_intn_h.int16_t;  -- /home/andrew/src/vgmstream-r1721/src/streamtypes.h:41

   subtype sample_t is bits_stdint_intn_h.int16_t;  -- /home/andrew/src/vgmstream-r1721/src/streamtypes.h:42

end streamtypes_h;
