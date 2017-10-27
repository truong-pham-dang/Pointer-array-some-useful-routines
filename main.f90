program Pointer_array
    use mod_pt_array
    use mod_pt_array_utils
    implicit none

    integer                :: i, posi
    real                   :: x
    type(element), pointer :: pt_elem
    type(obj_elem)         :: elem_obj

    nbelements = 20

    call random_seed()

    allocate(elements(1:nbelements))
    do i = 1, nbelements
        allocate(elements(i)%p)
        pt_elem => elements(i)%p
        call random_number(x)
        pt_elem%ident  = i
        pt_elem%value_elem = x
    end do

    write(*,*) 'Ids and values of elements:'
    do i = 1, nbelements
        pt_elem => elements(i)%p
        elem_obj%p => pt_elem
        call elem_obj%print_elem
    end do

    do while (posi /= 0)
        write(*,*) 'Please enter the position to delete an element in pointer array:'
        write(*,*) '(Program will be terminated when typing "0").'
        read(*,*) posi
        if (posi == 0) then
            stop 'Good bye.'
        end if

        call delete_elements_at_position(posi)

        write(*,*) 'Ids and values of elements after deleting:'
        do i = 1, nbelements
            pt_elem => elements(i)%p
            elem_obj%p => pt_elem
            call elem_obj%print_elem
        end do
    end do


end program Pointer_array
