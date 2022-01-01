module m_str
    implicit none

    interface str
        module procedure int_to_str
        module procedure logical_to_str
        module procedure real_to_str
        module procedure double_to_str
    end interface

    private
    public str

contains

    function int_to_str(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(100) :: buf

        write (buf, '(i0)') i
        s = trim(buf)
    end function

    function real_to_str(r) result(s)
        real, intent(in) :: r
        character(:), allocatable :: s
        character(100) :: buf

        write (buf, '(e14.4)') r
        s = trim(buf)
    end function

    function double_to_str(d) result(s)
        double precision, intent(in) :: d
        character(:), allocatable :: s
        character(100) :: buf

        write (buf, '(e14.4)') d
        s = trim(buf)
    end function

    function logical_to_str(logic) result(s)
        logical, intent(in) :: logic
        character(:), allocatable :: s
        if (logic) then
            s = 'true'
        else
            s = 'false'
        end if
    end function

end module
