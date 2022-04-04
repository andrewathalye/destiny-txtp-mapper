pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;

package bits_types_u_mbstate_t_h is

  -- Integral type unchanged by default argument promotions that can
  --   hold any value corresponding to members of the extended character
  --   set, as well as at least one value that does not correspond to any
  --   member of the extended character set.   

  -- Conversion state information.   
  -- Value so far.   
   subtype uu_mbstate_t_array1006 is Interfaces.C.char_array (0 .. 3);
   type uu_mbstate_t_union1183 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_wch : aliased unsigned;  -- /usr/include/bits/types/__mbstate_t.h:18
         when others =>
            uu_wchb : aliased uu_mbstate_t_array1006;  -- /usr/include/bits/types/__mbstate_t.h:19
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type uu_mbstate_t is record
      uu_count : aliased int;  -- /usr/include/bits/types/__mbstate_t.h:15
      uu_value : aliased uu_mbstate_t_union1183;  -- /usr/include/bits/types/__mbstate_t.h:20
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/types/__mbstate_t.h:21

end bits_types_u_mbstate_t_h;
