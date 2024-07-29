module sfincs_bmi
   use iso_c_binding
   use sfincs_lib
   use sfincs_ncoutput
   use sfincs_data
   
   implicit none
   private

   ! routines
   public :: initialize
   public :: update
   public :: update_until
   public :: finalize
   public :: get_value_ptr
   public :: get_var_shape
   public :: get_var_type
   public :: get_var_rank
   public :: get_start_time
   public :: get_end_time
   public :: get_time_step
   public :: get_current_time

   ! constants
   public :: BMI_LENVARADDRESS
   public :: BMI_LENVARTYPE
   public :: BMI_LENGRIDTYPE
   public :: BMI_LENCOMPONENTNAME
   public :: BMI_LENVERSION
   public :: BMI_LENERRMESSAGE

   integer(c_int), bind(C, name="BMI_LENVARADDRESS") :: BMI_LENVARADDRESS = 64 !< max. length for the variable's address C-string, also the variable name
   !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENVARADDRESS
   integer(c_int), bind(C, name="BMI_LENVARTYPE") :: BMI_LENVARTYPE = 64 !< max. length for variable type C-strings
   !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENVARTYPE
   integer(c_int), bind(C, name="BMI_LENGRIDTYPE") :: BMI_LENGRIDTYPE = 64 !< max. length for grid type C-strings
   !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENGRIDTYPE
   integer(c_int), bind(C, name="BMI_LENCOMPONENTNAME") :: BMI_LENCOMPONENTNAME = 64 !< component name length, i.e. 'SFINCS'
   !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENCOMPONENTNAME
   integer(c_int), bind(C, name="BMI_LENVERSION") :: BMI_LENVERSION = 256 !< length of version string, e.g. '3.1' or '16.4 release candidate'
   !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENVERSION
   integer(c_int), bind(C, name="BMI_LENERRMESSAGE") :: BMI_LENERRMESSAGE = 1024 !< max. length of error message
   !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENERRMESSAGE

contains
   
   function initialize() result(ierr) bind(C, name="initialize")
   !DEC$ ATTRIBUTES DLLEXPORT :: initialize
      integer(kind=c_int) :: ierr

      ierr = sfincs_initialize()

   end function initialize
   
   function update() result(ierr) bind(C, name="update")
   !DEC$ ATTRIBUTES DLLEXPORT :: update
      integer(kind=c_int) :: ierr
      real*8 :: unused_t
      
      unused_t = -1.0
      ierr = sfincs_update(unused_t)
   
   end function update
   
   function update_until(t_target) result(ierr) bind(C, name="update_until")
   !DEC$ ATTRIBUTES DLLEXPORT :: update_until  
      real(kind=c_double), value, intent(in) :: t_target
      integer(kind=c_int) :: ierr

      real(kind=c_double) :: delta_t
      
      delta_t = t_target - t
      ierr = sfincs_update(delta_t)
      
   end function update_until
   
   function finalize() result(ierr) bind(C, name="finalize")
   !DEC$ ATTRIBUTES DLLEXPORT :: finalize
   integer(kind=c_int) :: ierr
   
   ierr = sfincs_finalize()
   
   end function finalize

   function get_value_ptr(c_var_name, c_data) result(ierr) &
      bind(C, name="get_value_ptr")
   !DIR$ ATTRIBUTES DLLEXPORT :: get_value_ptr
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(inout) :: c_data
      integer(kind=c_int) :: ierr
      character(len=strlen(c_var_name, BMI_LENVARADDRESS)) :: var_name
      integer :: c_strlen

      ierr = 0

      c_strlen = strlen(c_var_name, BMI_LENVARADDRESS)
      var_name = char_array_to_string(c_var_name, c_strlen)

      select case(var_name)
      case("z_xz")
         c_data = c_loc(z_xz)
      case("z_yz")
         c_data = c_loc(z_yz)
      case("zs")
         c_data = c_loc(zs)
      case("zb")
         c_data = c_loc(zb)
      case("subgrid_z_zmin")
         c_data = c_loc(subgrid_z_zmin)
      case("cumprcpt")
         c_data = c_loc(cumprcpt)
      case default
         c_data = c_null_ptr
         ierr = -1
      end select

   end function get_value_ptr
   
   function get_var_shape(c_var_name, var_shape) result(ierr) &
      bind(C, name="get_var_shape")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape
      character(kind=c_char), intent(in) :: c_var_name(*)
      integer(c_int), intent(inout) :: var_shape(6)
      character(len=strlen(c_var_name, BMI_LENVARADDRESS)) :: var_name
      integer(kind=c_int) :: ierr

      ierr = 0

      var_name = char_array_to_string(c_var_name, strlen(c_var_name, BMI_LENVARADDRESS))
      var_shape = (/0, 0, 0, 0, 0, 0/)
      
      select case(var_name)
      case("z_xz", "z_yz", "zs", "zb")
         var_shape(1) = size(zs)
      case("subgrid_z_zmin")
         var_shape(1) = size(subgrid_z_zmin)
      case("z_index_z_n", "z_index_z_m")
         var_shape(1) = size(z_index_z_n)
      case("cumprcpt")
         var_shape(1) = size(cumprcpt)
      case default
         ierr = -1
      end select
   
   end function get_var_shape
   
   function get_var_type(c_var_name, c_type) result(ierr) &
      bind(C, name="get_var_type")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_var_type   
      character(kind=c_char), intent(in) :: c_var_name(*)
      character(kind=c_char), intent(out) ::  c_type(BMI_LENVARTYPE)
      character(len=BMI_LENVARADDRESS-1) :: var_name
      character(len=BMI_LENVARTYPE-1) :: type_name

      integer(kind=c_int)              :: ierr

      ierr = 0
      var_name = char_array_to_string(c_var_name, strlen(c_var_name, BMI_LENVARADDRESS))

      select case(var_name)
      case("z_xz", "z_yz", "zs", "zb", "subgrid_z_zmin", "cumprcpt")
         type_name = "float"
      case("z_index_z_n", "z_index_z_m")
         type_name = "integer"
      case default
         ierr = -1
      end select
      
      c_type = string_to_char_array(trim(type_name), len(trim(type_name)))
   
   end function get_var_type
   
   function get_var_rank(c_var_name, rank) result(ierr) &
      bind(C, name="get_var_rank")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_var_rank
      character(kind=c_char), intent(in) :: c_var_name(*)
      integer(c_int), intent(out) :: rank
      integer(kind=c_int) :: ierr

      character(len=BMI_LENVARADDRESS-1) :: var_name
      
      ierr = 0

      var_name = char_array_to_string(c_var_name, strlen(c_var_name, BMI_LENVARADDRESS))
      
      select case(var_name)
      case("z_xz", "z_yz", "zs", "zb", "subgrid_z_zmin", "cumprcpt")
         rank = 1
      case default
         ierr = -1
      end select

   end function get_var_rank
      
   function get_start_time(tstart) result(ierr) bind(C, name="get_start_time")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
      real(c_double), intent(out) :: tstart
      integer(kind=c_int) :: ierr

      tstart = t0
      ierr = 0

   end function get_start_time
      
   function get_end_time(tend) result(ierr) bind(C, name="get_end_time")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time   
      real(c_double), intent(out) :: tend
      integer(kind=c_int) :: ierr

      tend = t1
      ierr = 0

   end function get_end_time
      
   function get_time_step(deltat) result(ierr) bind(C, name="get_time_step")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step
      real(c_double), intent(out) :: deltat
      integer(kind=c_int) :: ierr

      deltat = dt
      ierr = 0

   end function get_time_step
      
   function get_current_time(tcurrent) result(ierr) bind(C, name="get_current_time")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
      real(c_double), intent(out) :: tcurrent
      integer(kind=c_int) :: ierr

      tcurrent = t
      ierr = 0

   end function get_current_time

   !> @brief Get the last error in the BMI as a character array
   !! with size BMI_LENERRMESSAGE
   !<
   function get_last_bmi_error(c_error) result(ierr) &
      bind(C, name="get_last_bmi_error")
   !DIR$ ATTRIBUTES DLLEXPORT :: get_last_bmi_error
      character(kind=c_char, len=1), intent(out) :: c_error(BMI_LENERRMESSAGE)
      integer(kind=c_int) :: ierr

      c_error = string_to_char_array("error handling not implemented", 30)
      ierr = 0

   end function get_last_bmi_error
   
   !> @brief Returns the string length without the trailing null character
   !<
   pure function strlen(char_array, max_len) result(string_length)
      integer, intent(in) :: max_len
      character(c_char), intent(in) :: char_array(max_len) !< C-style character string
      integer :: string_length !< Fortran string length
      integer :: i

      string_length = 0
      do i = 1, size(char_array)
         if (char_array(i) .eq. C_NULL_CHAR) then
            string_length = i - 1
            exit
         end if
      end do

   end function strlen

   !> @brief Convert C-style string to Fortran character string
   !<
   pure function char_array_to_string(char_array, length) result(f_string)
      integer(c_int), intent(in) :: length !< string length without terminating null character
      character(c_char), intent(in) :: char_array(length) !< string to convert
      character(len=length) :: f_string !< Fortran fixed length character string
      integer :: i

      do i = 1, length
         f_string(i:i) = char_array(i)
      end do

   end function char_array_to_string

   !> @brief Convert Fortran string to C-style character string
   !<
   pure function string_to_char_array(string, length) result(c_array)
      integer(c_int), intent(in) :: length !< Fortran string length
      character(len=length), intent(in) :: string !< string to convert
      character(kind=c_char, len=1) :: c_array(length + 1) !< C-style character string
      integer :: i

      do i = 1, length
         c_array(i) = string(i:i)
      end do
      c_array(length + 1) = C_NULL_CHAR

   end function string_to_char_array

end module sfincs_bmi