program sfincs
   !
   use sfincs_lib
   !
   implicit none
   !
   integer :: ierr
   double precision    :: deltat
   !
   deltat = -1.0
   ierr = 0
   !
   if (ierr == 0) then
      ierr = sfincs_initialize()
   endif
   !
   if (ierr == 0) then
      ierr = sfincs_update(deltat)
   endif
   !
   ! Always finalize, especially in case of error
   !
   ierr = sfincs_finalize()
   !
   call exit(ierr)
   !
end program
