module m_timer_mpi
    use, intrinsic :: iso_fortran_env, only: dp => real64, int8
    use mpi
    use stdlib_hashmap_wrappers, only: key_type
    use stdlib_hashmaps, only: chaining_hashmap_type

    implicit none

    private
    public :: t_TimerMPI, t_TimerManager

    integer, parameter :: max_label_len = 64
    integer, parameter :: max_entry_len = 256

    type :: t_TimerMPI
        integer           :: n_calls = 0
        real(dp)          :: t0 = 0.0_dp
        real(dp)          :: tsum = 0.0_dp
        real(dp)          :: tmin = huge(0.0_dp)
        real(dp)          :: tmax = 0.0_dp
    contains
        procedure :: start => tstat_start
        procedure :: stop => tstat_stop
        procedure :: reset => tstat_reset
        procedure :: save_mpiio => tstat_save_mpiio
    end type

    type :: t_TimerManager
        type(chaining_hashmap_type) :: name2timer
        character(len=100), dimension(30) :: names
    contains
        procedure :: init => timer_manager_init
        procedure :: add_timer => timer_manager_add_timer
        procedure :: start_timer => timer_manager_start_timer
        procedure :: stop_timer => timer_manager_stop_timer
        procedure :: save_all_mpiio => timer_manager_save_all_mpiio
    end type

contains

    subroutine tstat_start(self, sync)
        class(t_TimerMPI), intent(inout) :: self
        logical, intent(in), optional :: sync

        integer :: ierr

        if (present(sync)) then
            if (sync) call MPI_Barrier(MPI_COMM_WORLD, ierr)
        end if

        self%t0 = MPI_Wtime()
    end subroutine

    subroutine tstat_stop(self)
        class(t_TimerMPI), intent(inout) :: self

        real(dp) :: dt

        dt = MPI_Wtime() - self%t0

        self%n_calls = self%n_calls + 1
        self%tsum = self%tsum + dt
        self%tmin = min(self%tmin, dt)
        self%tmax = max(self%tmax, dt)
    end subroutine

    subroutine tstat_reset(self)
        class(t_TimerMPI), intent(inout) :: self

        self%n_calls = 0
        self%t0 = 0
        self%tsum = 0
        self%tmin = huge(0.0_dp)
        self%tmax = 0
    end subroutine

    subroutine tstat_save_mpiio(self, label, filename, comm)
        class(t_TimerMPI), intent(in) :: self
        character(*), intent(in) :: label
        character(*), intent(in) :: filename
        integer, intent(in), optional :: comm

        integer :: irank, comm_used, ierr, fh

        integer(MPI_OFFSET_KIND) :: offset
        character(len=max_entry_len) :: line

        comm_used = merge(comm, MPI_COMM_WORLD, present(comm))
        call MPI_Comm_rank(comm_used, irank, ierr)

        write(line, '(A,",",I0,",",ES14.7,",",ES14.7,",",ES14.7,",",ES14.7)') trim(label), self%n_calls, self%tsum, self%tsum/self%n_calls, self%tmin, self%tmax

        ! 固定長にする
        line = adjustl(line)
        line = line(:len_trim(line))//repeat(" ", max_entry_len - len_trim(line))

        offset = int(irank, kind=MPI_OFFSET_KIND)*max_entry_len

        call MPI_File_open(comm_used, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, fh, ierr)
        call MPI_File_write_at(fh, offset, line, max_entry_len, MPI_CHARACTER, MPI_STATUS_IGNORE, ierr)
        call MPI_File_close(fh, ierr)
    end subroutine

    subroutine timer_manager_init(self)
        class(t_TimerManager), intent(inout) :: self

        call self%name2timer%init()
    end subroutine

    subroutine timer_manager_add_timer(self, name)
        class(t_TimerManager), intent(inout) :: self
        character(*), intent(in) :: name

        type(t_TimerMPI) :: new_timer

        call self%name2timer%map_entry(trim(name), new_timer)
        self%names(self%name2timer%entries()) = name
    end subroutine

    subroutine timer_manager_start_timer(self, name)
        class(t_TimerManager), intent(inout) :: self
        character(*), intent(in) :: name

        class(*), allocatable :: timer
        logical :: exists

        call self%name2timer%get_other_data(trim(name), timer, exists)

        if (.not. exists) then
            call self%add_timer(name)
            call self%name2timer%get_other_data(trim(name), timer)
        end if

        select type (timer)
        type is (t_TimerMPI)
            call timer%start()
        end select
    end subroutine

    subroutine timer_manager_stop_timer(self, name)
        class(t_TimerManager), intent(inout) :: self
        character(*), intent(in) :: name

        class(*), allocatable :: timer
        logical :: exists

        call self%name2timer%get_other_data(trim(name), timer, exists)

        if (.not. exists) return

        select type (timer)
        type is (t_TimerMPI)
            call timer%stop()
        end select
    end subroutine

    subroutine timer_manager_save_all_mpiio(self, filename, comm)
        class(t_TimerManager), intent(inout) :: self
        character(*), intent(in) :: filename
        integer, intent(in), optional :: comm

        class(*), allocatable :: timer
        logical :: exists
        integer :: i

        integer(int8) :: n_entries

        do i = 1, self%name2timer%entries()
            call self%name2timer%get_other_data(self%names(i), timer, exists)

            if (.not. exists) return

            select type (timer)
            type is (t_TimerMPI)
                call timer%save_mpiio(self%names(i), filename, comm)
            end select
        end do
    end subroutine

end module
