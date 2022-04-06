pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package g72x_state_h is

  -- * g72x_state.h - internal state used by g721 decoder
  --  

  -- Locked or steady state step size multiplier.  
   type anon1656_array1657 is array (0 .. 1) of aliased short;
   type anon1656_array1659 is array (0 .. 5) of aliased short;
   type g72x_state is record
      yl : aliased long;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:10
      yu : aliased short;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:11
      dms : aliased short;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:12
      dml : aliased short;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:13
      ap : aliased short;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:14
      a : aliased anon1656_array1657;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:16
      b : aliased anon1656_array1659;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:17
      pk : aliased anon1656_array1657;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:18
      dq : aliased anon1656_array1659;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:22
      sr : aliased anon1656_array1657;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:27
      td : aliased char;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:32
   end record
   with Convention => C_Pass_By_Copy;  -- /home/andrew/src/vgmstream-r1721/src/coding/g72x_state.h:9

  -- Unlocked or non-steady state step size multiplier.  
  -- Short term energy estimate.  
  -- Long term energy estimate.  
  -- Linear weighting coefficient of 'yl' and 'yu'.  
  -- Coefficients of pole portion of prediction filter.  
  -- Coefficients of zero portion of prediction filter.  
  --             * Signs of previous two samples of a partially
  --             * reconstructed signal.
  --              

  --             * Previous 6 samples of the quantized difference
  --             * signal represented in an internal floating point
  --             * format.
  --              

  --             * Previous 2 samples of the quantized difference
  --             * signal represented in an internal floating point
  --             * format.
  --              

  -- delayed tone detect, new in 1988 version  
end g72x_state_h;
