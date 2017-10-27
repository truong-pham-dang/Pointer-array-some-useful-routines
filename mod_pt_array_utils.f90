module mod_pt_array_utils
    use mod_pt_array
    implicit none

    type :: obj_elem
        type(element), pointer :: p
        contains
        procedure :: print_elem => print_element
!        procedure :: constr_elem => element_construtor
    end type
    contains

    subroutine print_element(this)
    implicit none
    class(obj_elem), intent(in) :: this

    write(*,20), this%p%ident, this%p%value_elem
    20 format(i5xf15.7)
    end subroutine print_element

!    subroutine element_constructor(this,i,x)
!    implicit none
!    class(obj_elem), intent(in) :: this
!    integer, intent(in)         :: i
!    real, intent(in)            :: x
!
!    this%p%ident = i
!    this%p%value_elem = x
!    end subroutine element_constructor

    subroutine delete_elements_at_position(posi)
    implicit none
    integer, intent(in) :: posi
    integer             :: i

    do i = posi, nbelements - 1
        elements(i)%p => elements(i+1)%p
    end do

    nbelements = nbelements - 1

    end subroutine delete_elements_at_position

end module
