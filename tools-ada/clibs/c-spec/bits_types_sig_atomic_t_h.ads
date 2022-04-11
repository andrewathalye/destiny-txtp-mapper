pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_types_h;

package bits_types_sig_atomic_t_h is

  -- An integral type that can be modified atomically, without the
  --   possibility of a signal arriving in the middle of the operation.   

   subtype sig_atomic_t is bits_types_h.uu_sig_atomic_t;  -- /usr/include/bits/types/sig_atomic_t.h:8

end bits_types_sig_atomic_t_h;
