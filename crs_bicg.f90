module crs_matrix
    type crs
        complex(8), allocatable :: val(:)
        integer, allocatable :: col_int(:), row_ptr(:)
    end type crs
end module crs_matrix

program main
    use crs_matrix
    implicit none
    type(crs) :: matrix
    integer :: matrix_size
    real(8) :: gamma
    character :: arg*10

    call getarg(1, arg)
    read(arg, *) matrix_size
    call getarg(2, arg)
    read(arg, *) gamma

    print *, matrix_size
    print *, gamma

    call init(matrix, matrix_size, gamma)

    print *, "val=", matrix%val
    print *, "col=", matrix%col_int
    print *, "row=", matrix%row_ptr

    deallocate(matrix%val, matrix%col_int, matrix%row_ptr)

contains

    subroutine init(matrix, matrix_size, gamma)
        type(crs), intent(out) :: matrix
        integer, intent(in) :: matrix_size
        real(8), intent(in) :: gamma
        integer :: i, j
        integer :: val_num

        val_num = matrix_size * 3 - 2
        allocate(matrix%val(val_num), matrix%col_int(val_num), matrix%row_ptr(matrix_size + 1))

        j = 1
        do i = 1, matrix_size
            matrix%row_ptr(i) = j
            if (i > 1) then
                matrix%val(j) = gamma
                matrix%col_int(j) = i - 1
                j = j + 1
            end if
            matrix%val(j) = 2
            matrix%col_int(j) = i
            j = j + 1
            if (i < matrix_size) then
                matrix%val(j) = 1
                matrix%col_int(j) = i + 1
                j = j + 1
            end if
        end do
        matrix%row_ptr(matrix_size + 1) = j
    end subroutine init
end program main
