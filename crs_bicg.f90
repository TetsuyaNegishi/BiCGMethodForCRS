module crs_matrix
    type crs
        complex(8), allocatable :: val
        integer, allocatable :: col_int, row_ptr
    end type ers
end module crs_matrix

program main
    use crs_matrix
    implicit none
    integer matrix_size
    real(8) gamma
    character :: arg*10

    call getarg(1, arg)
    read(arg, *) matrix_size
    call getarg(2, arg)
    read(arg, *) gamma

    print *, matrix_size
    print *, gamma
end program main
