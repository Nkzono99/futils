module m_get_default
    implicit none

    interface get_default
        module procedure &
            get_default_ii, &
            get_default_rr, &
            get_default_dd, &
            get_default_ll, &
            get_default_xx, &
            get_default_dxdx, &
            get_default_cc
    end interface

contains

    elemental function get_default_ii(i, default) result(ret)
        integer, intent(in), optional :: i
        integer, intent(in) :: default
        integer :: ret

        if (present(i)) then
            ret = i
        else
            ret = default
        end if
    end function

    elemental function get_default_rr(i, default) result(ret)
        real, intent(in), optional :: i
        real, intent(in) :: default
        real :: ret

        if (present(i)) then
            ret = i
        else
            ret = default
        end if
    end function

    elemental function get_default_dd(d, default) result(ret)
        double precision, intent(in), optional :: d
        double precision, intent(in) :: default

        double precision :: ret

        if (present(d)) then
            ret = d
        else
            ret = default
        end if
    end function

    elemental function get_default_ll(logic, default) result(ret)
        logical, intent(in), optional :: logic
        logical, intent(in) :: default
        logical :: ret

        if (present(logic)) then
            ret = logic
        else
            ret = default
        end if
    end function

    elemental function get_default_xx(comp, default) result(ret)
        complex, intent(in), optional :: comp
        complex, intent(in) :: default
        complex :: ret

        if (present(comp)) then
            ret = comp
        else
            ret = default
        end if
    end function

    elemental function get_default_dxdx(comp, default) result(ret)
        double complex, intent(in), optional :: comp
        double complex, intent(in) :: default
        double complex :: ret

        if (present(comp)) then
            ret = comp
        else
            ret = default
        end if
    end function

    elemental function get_default_cc(c, default) result(ret)
        character, intent(in), optional :: c
        character, intent(in) :: default
        character :: ret

        if (present(c)) then
            ret = c
        else
            ret = default
        end if
    end function

end module
