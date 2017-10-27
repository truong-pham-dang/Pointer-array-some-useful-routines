module mod_pt_array
    implicit none
! the type of element can contain coordinates of vertexes, coordinate of centroid cell ...
    type :: element
        integer :: ident = 0
        real    :: value_elem = 0.0
    end type

    type :: element_pt
        type(element), pointer :: p => null()
    end type

    type(element_pt), dimension(:), allocatable :: elements

    integer :: nbelements
end module
