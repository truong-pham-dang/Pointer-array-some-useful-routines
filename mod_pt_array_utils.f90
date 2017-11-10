module mod_pt_array_utils
    use mod_pt_array
    implicit none

    type :: obj_elem
        type(element), pointer :: p
        contains
        procedure :: print_elem => print_element
    end type

    interface element
        module procedure element_constructor
    end interface

    contains

    subroutine print_element(this)
    implicit none
    class(obj_elem), intent(in) :: this

    write(*,20), this%p%ident, this%p%value_elem
    20 format(i5xf15.7)
    end subroutine print_element

    type(element) function element_constructor(i,x)
    implicit none
    integer, intent(in)         :: i
    real,    intent(in)         :: x

    element_constructor%ident = i
    element_constructor%value_elem = x
    end function element_constructor


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
