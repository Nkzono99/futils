module m_list
    implicit none

    integer, parameter :: DEFAULT_MAX_N = 30
    double precision, parameter :: DEFAULT_GROWTH_FACTOR = 0.5d0*(1.0d0 + dsqrt(5.0d0))

    type, abstract :: t_List
        integer :: current_size = 0
        ! type(TYPE), pointer :: buffer(:) => null()
        ! type(TYPE), pointer :: tmp_buffer(:) => null()

        integer :: max_size = DEFAULT_MAX_N
        double precision :: growth_factor = DEFAULT_GROWTH_FACTOR

    contains

        procedure(list_allocate_tmp_buffer), deferred :: allocate_tmp_buffer
        procedure(list_copy_to_tmp_buffer), deferred :: copy_to_tmp_buffer
        procedure(list_switch_to_tmp_buffer), deferred :: switch_to_tmp_buffer
        procedure(list_destroy), deferred :: destroy
        procedure :: extent_size => list_extent_size

    end type

    interface
        subroutine list_allocate_tmp_buffer(self, n)
            import t_List
            class(t_List), intent(inout) :: self
            integer, intent(in) :: n

            ! allocate (self%tmp_buffer(n))
        end subroutine

        subroutine list_copy_to_tmp_buffer(self)
            import t_List
            class(t_List), intent(inout) :: self

            ! integer :: i

            ! do i = 1, self%current_size
            !     self%tmp_buffer(i) = self%buffer(i)
            ! end do

            ! deallocate (self%buffer)
        end subroutine

        subroutine list_switch_to_tmp_buffer(self)
            import t_List
            class(t_List), intent(inout) :: self

            ! if (associated(self%buffer)) then
            !     deallocate (self%buffer)
            ! end if
            ! self%buffer => self%tmp_buffer
            ! nullify (self%tmp_buffer)
        end subroutine

        subroutine list_destroy(self)
            import t_List
            class(t_List), intent(inout) :: self

            ! deallocate(self%buffer)
        end subroutine
    end interface

    private
    public t_List
    public init_list

contains

    subroutine init_list(self, max_size, growth_factor)
        class(t_List), intent(inout) :: self
        integer, intent(in), optional :: max_size
        double precision, intent(in), optional :: growth_factor

        if (present(max_size)) then
            self%max_size = max_size
        else
            self%max_size = DEFAULT_MAX_N
        end if

        if (present(growth_factor)) then
            self%growth_factor = growth_factor
        else
            self%growth_factor = DEFAULT_GROWTH_FACTOR
        end if

        self%current_size = 0

        call self%allocate_tmp_buffer(self%max_size)
        call self%switch_to_tmp_buffer()
    end subroutine

    subroutine list_extent_size(self)
        class(t_List), intent(inout) :: self

        integer :: new_max_n

        new_max_n = max( &
                    int(self%max_size*self%growth_factor), &
                    self%max_size + 1 &
                    )

        call self%allocate_tmp_buffer(new_max_n)
        call self%copy_to_tmp_buffer()
        call self%switch_to_tmp_buffer()

        self%max_size = new_max_n
    end subroutine
end module

! !> List inheritance template.
! module m_extends_list
!     use m_list, only: t_List, init_list
!     implicit none

!     type :: t_Element
!     end type

!     type, extends(t_List) :: t_ElementList
!         type(t_Element), pointer :: buffer(:) => null()
!         type(t_Element), pointer :: tmp_buffer(:) => null()

!     contains
!         procedure :: allocate_tmp_buffer => elementList_allocate_tmp_buffer
!         procedure :: copy_to_tmp_buffer => elementList_copy_to_tmp_buffer
!         procedure :: switch_to_tmp_buffer => elementList_switch_to_tmp_buffer
!         procedure :: destroy => elementList_destroy
!         procedure :: append => elementList_append
!         procedure :: get => elementList_get
!     end type

!     private
!     public t_ElementList
!     public new_ElementList

! contains

!     function new_ElementList(max_size, growth_factor) result(obj)
!         type(t_ElementList) :: obj
!         integer, intent(in), optional :: max_size
!         double precision, intent(in), optional :: growth_factor

!         call init_list(obj, max_size, growth_factor)
!     end function

!     subroutine elementList_allocate_tmp_buffer(self, n)
!         class(t_ElementList), intent(inout) :: self
!         integer, intent(in) :: n

!         allocate (self%tmp_buffer(n))
!     end subroutine

!     subroutine elementList_copy_to_tmp_buffer(self)
!         class(t_ElementList), intent(inout) :: self

!         integer :: i

!         do i = 1, self%current_size
!             self%tmp_buffer(i) = self%buffer(i)
!         end do

!         deallocate (self%buffer)
!     end subroutine

!     subroutine elementList_switch_to_tmp_buffer(self)
!         class(t_ElementList), intent(inout) :: self

!         if (associated(self%buffer)) then
!             deallocate (self%buffer)
!         end if
!         self%buffer => self%tmp_buffer
!         nullify (self%tmp_buffer)
!     end subroutine

!     subroutine elementList_append(self, element)
!         class(t_ElementList), intent(inout) :: self
!         type(t_Element), intent(in) :: element

!         if (self%current_size == self%max_size) then
!             call self%extent_size()
!         end if

!         self%current_size = self%current_size + 1
!         self%buffer(self%current_size) = element
!     end subroutine

!       subroutine elementList_destroy(self)
!           class(t_ElementList), intent(inout) :: self

!           deallocate(self%buffer)
!       end subroutine

!       function elementList_get(self, i) result(ret)
!           class(t_ElementList), intent(in) :: self
!           integer, intent(in) :: i
!           type(t_Element) :: ret

!           ret = self%buffer(i)
!       end function

! end module
