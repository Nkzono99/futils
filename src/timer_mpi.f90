module m_timer_mpi
    use, intrinsic :: iso_fortran_env
    use mpi

    implicit none
    private
    public :: t_TimerMPI

    type :: t_TimerMPI
        integer           :: n_calls = 0
        real(real64)      :: t0 = 0.0_real64
        real(real64)      :: tsum = 0.0_real64
        real(real64)      :: tmin = huge(0.0_real64)
        real(real64)      :: tmax = 0.0_real64
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
        real(real64) :: dt
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
        real(real64)    :: buf(4), g(4)     ! n, tsum, tmin, tmax をまとめて Reduce
        integer         :: irank

        integer :: ierr

        c = MPI_COMM_WORLD
        c = merge(comm, MPI_COMM_WORLD, present(comm))
        call MPI_Comm_rank(c, irank, ierr)

        buf = [real(self%n_calls, real64), self%tsum, self%tmin, self%tmax]
        call MPI_Reduce(buf, g, 4, MPI_REAL8, MPI_SUM, 0, c)   ! n と tsum は合計
        if (irank == 0) then
            ! min/max は別途
            call MPI_Reduce(self%tmin, g(3), 1, MPI_REAL8, MPI_MIN, 0, c)
            call MPI_Reduce(self%tmax, g(4), 1, MPI_REAL8, MPI_MAX, 0, c)

            write (*, '(a,":  calls=",i8,"  total=",f10.6," s  avg=",f10.6,"  min=",f10.6,"  max=",f10.6)') &
                trim(merge(label, 'timer', present(label))), nint(g(1)), g(2), g(2)/g(1), g(3), g(4)
        end if
    end subroutine

end module
