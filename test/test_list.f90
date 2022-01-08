module m_integer_list
    use m_list, only: t_List, init_list
    implicit none

    type, extends(t_List) :: t_ElementList
        integer, pointer :: buffer(:) => null()
        integer, pointer :: tmp_buffer(:) => null()

    contains
        procedure :: allocate_tmp_buffer => ilist_allocate_tmp_buffer
        procedure :: copy_to_tmp_buffer => ilist_copy_to_tmp_buffer
        procedure :: switch_to_tmp_buffer => ilist_switch_to_tmp_buffer
        procedure :: destroy => ilist_destroy
        procedure :: append => ilist_append
    end type

contains

    function new_IntegerList(max_size, growth_factor) result(obj)
        type(t_ElementList) :: obj
        integer, intent(in), optional :: max_size
        double precision, intent(in), optional :: growth_factor

        call init_list(obj, max_size, growth_factor)
    end function

    subroutine ilist_allocate_tmp_buffer(self, n)
        class(t_ElementList), intent(inout) :: self
        integer, intent(in) :: n

        allocate (self%tmp_buffer(n))
    end subroutine

    subroutine ilist_copy_to_tmp_buffer(self)
        class(t_ElementList), intent(inout) :: self

        integer :: i

        do i = 1, self%current_size
            self%tmp_buffer(i) = self%buffer(i)
        end do

        deallocate (self%buffer)
    end subroutine

    subroutine ilist_switch_to_tmp_buffer(self)
        class(t_ElementList), intent(inout) :: self

        if (associated(self%buffer)) then
            deallocate (self%buffer)
        end if
        self%buffer => self%tmp_buffer
        nullify (self%tmp_buffer)
    end subroutine

    subroutine ilist_destroy(self)
        class(t_ElementList), intent(inout) :: self

        deallocate (self%buffer)
    end subroutine

    subroutine ilist_append(self, i)
        class(t_ElementList), intent(inout) :: self
        integer, intent(in) :: i

        if (self%current_size == self%max_size) then
            call self%extent_size
        end if

        self%current_size = self%current_size + 1
        self%buffer(self%current_size) = i
    end subroutine

end module

program test_list
    use m_integer_list
    print *, 'test_list_append'
    call test_list_append

contains

    subroutine test_list_append
        type(t_ElementList) :: ilist

        integer :: sum_list
        integer :: sum_list_correct

        ilist = new_IntegerList(max_size=10)

        sum_list_correct = 0
        do i = 1, 20
            call ilist%append(i)
            sum_list_correct = sum_list_correct + i
        end do

        sum_list = 0
        do i = 1, ilist%current_size
            sum_list = sum_list + ilist%buffer(i)
        end do

        if (sum_list == sum_list_correct) then
            print *, 'ok'
        else
            print *, 'AssersionError:', sum_list, '==', sum_list_correct, '(expected)'
        end if
    end subroutine

end program test_list
