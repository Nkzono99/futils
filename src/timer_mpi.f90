module m_timer_mpi
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use mpi

    implicit none

    private
    public :: t_TimerMPI

    type :: t_TimerMPI
        integer           :: n_calls = 0
        real(dp)      :: t0 = 0.0_dp
        real(dp)      :: tsum = 0.0_dp
        real(dp)      :: tmin = huge(0.0_dp)
        real(dp)      :: tmax = 0.0_dp
    contains
        procedure :: start => tstat_start
        procedure :: stop => tstat_stop
        procedure :: reset => tstat_reset
        procedure :: print_stats => tstat_print
    end type

contains

    subroutine tstat_start(self, sync)
        class(t_TimerMPI), intent(inout) :: self
        logical, intent(in), optional :: sync   ! true: Barrier 同期

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
        self.tmin = 0
        self.tmax = 0
    end subroutine

    subroutine tstat_print(self, label, comm)
        class(t_TimerMPI), intent(in)           :: self
        character(*), intent(in), optional :: label
        integer, intent(in), optional :: comm

        integer :: c
        integer :: irank
        integer :: ierr

        c = merge(comm, MPI_COMM_WORLD, present(comm))
        call MPI_Comm_rank(c, irank, ierr)

        if (irank == 0) then
            print *, "------- ", trim(merge(label, 'timer', present(label))), " -------"
            print *, "calls:", self%n_calls
            print *, "total:", self%tsum
            print *, "avg:", self%tsum/self%n_calls
            print *, "min:", self%tmin
            print *, "max", self%tmax
            print *, "---------------------------"
        end if
    end subroutine

end module
