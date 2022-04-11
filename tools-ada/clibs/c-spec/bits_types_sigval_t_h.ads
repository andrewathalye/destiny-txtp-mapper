pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_types_u_sigval_t_h;

package bits_types_sigval_t_h is

  -- To avoid sigval_t (not a standard type name) having C++ name
  --   mangling depending on whether the selected standard includes union
  --   sigval, it should not be defined at all when using a standard for
  --   which the sigval name is not reserved; in that case, headers should
  --   not include <bits/types/sigval_t.h> and should use only the
  --   internal __sigval_t name.   

   subtype sigval_t is bits_types_u_sigval_t_h.uu_sigval_t;  -- /usr/include/bits/types/sigval_t.h:16

end bits_types_sigval_t_h;
