pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with bits_types_u_sigval_t_h;
limited with bits_pthreadtypes_h;
with bits_types_h;

package bits_types_sigevent_t_h is

   --  unsupported macro: sigev_notify_function _sigev_un._sigev_thread._function
   --  unsupported macro: sigev_notify_attributes _sigev_un._sigev_thread._attribute
  -- Forward declaration.   
  -- Structure to transport application-defined values with signals.   
  -- When SIGEV_SIGNAL and SIGEV_THREAD_ID set, LWP ID of the
  --	   thread to receive the signal.   

  -- Function to start.   
   type anon1083_array1086 is array (0 .. 11) of aliased int;
   type anon1083_struct1087 is record
      u_function : access procedure (arg1 : bits_types_u_sigval_t_h.uu_sigval_t);  -- /usr/include/bits/types/sigevent_t.h:38
      u_attribute : access bits_pthreadtypes_h.pthread_attr_t;  -- /usr/include/bits/types/sigevent_t.h:39
   end record
   with Convention => C_Pass_By_Copy;
   type anon1083_union1084 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            u_pad : aliased anon1083_array1086;  -- /usr/include/bits/types/sigevent_t.h:30
         when 1 =>
            u_tid : aliased bits_types_h.uu_pid_t;  -- /usr/include/bits/types/sigevent_t.h:34
         when others =>
            u_sigev_thread : aliased anon1083_struct1087;  -- /usr/include/bits/types/sigevent_t.h:40
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type sigevent is record
      sigev_value : aliased bits_types_u_sigval_t_h.uu_sigval_t;  -- /usr/include/bits/types/sigevent_t.h:24
      sigev_signo : aliased int;  -- /usr/include/bits/types/sigevent_t.h:25
      sigev_notify : aliased int;  -- /usr/include/bits/types/sigevent_t.h:26
      u_sigev_un : aliased anon1083_union1084;  -- /usr/include/bits/types/sigevent_t.h:41
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/bits/types/sigevent_t.h:22

  -- Thread attributes.   
   subtype sigevent_t is sigevent;  -- /usr/include/bits/types/sigevent_t.h:42

  -- POSIX names to access some of the members.   
end bits_types_sigevent_t_h;
