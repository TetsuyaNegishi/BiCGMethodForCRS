module crs_matrix
    type crs
        complex(8), allocatable :: val(:)
        integer, allocatable :: col_ind(:), row_ptr(:)
    end type crs

contains

    function matvec(matrix, vec) result(r)
        type(crs), intent(in) :: matrix
        complex(8), intent(in) :: vec(:)
        complex(8), allocatable :: r(:)
        integer :: i, j, dim

        dim = size(vec)
        allocate(r(dim))

        r = 0.0d0
        do i = 1, dim
            do j = matrix%row_ptr(i), matrix%row_ptr(i + 1) - 1
                r(i) = r(i) + matrix%val(j) * vec(matrix%col_ind(j))
            end do
        end do
    end function matvec

    function hermitian(matrix) result(r)
        type(crs), intent(in) :: matrix
        type(crs) :: r
        integer :: col, row, matrix_val_num, r_val_num, val_size, dim

        val_size = size(matrix%val)
        dim = size(matrix%row_ptr) - 1

        allocate(r%val(val_size), r%col_ind(val_size), r%row_ptr(dim + 1))

        r_val_num = 1
        do col = 1, dim
            r%row_ptr(col) = r_val_num
            do matrix_val_num = 1, val_size
                if(matrix%col_ind(matrix_val_num) == col) then
                    r%val(r_val_num) = matrix%val(matrix_val_num)
                    r%col_ind(r_val_num) = get_row(matrix_val_num, matrix%row_ptr)
                    r_val_num = r_val_num + 1
                end if
            end do
        end do
        r%row_ptr(dim + 1) = r_val_num

    contains

        function get_row(val_num, row_ptr) result(r)
            integer, intent(in) :: val_num
            integer, intent(in) :: row_ptr(:)
            integer :: r
            integer :: row, dim

            dim = size(row_ptr)
            do row = 1, dim
                if(val_num < row_ptr(row)) then
                    r = row - 1
                    exit
                end if
            end do
        end function get_row

    end function hermitian
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
    print *, "col=", matrix%col_ind
    print *, "row=", matrix%row_ptr

    deallocate(matrix%val, matrix%col_ind, matrix%row_ptr)

contains

    subroutine init(matrix, matrix_size, gamma)
        type(crs), intent(out) :: matrix
        integer, intent(in) :: matrix_size
        real(8), intent(in) :: gamma
        integer :: i, j
        integer :: val_num

        val_num = matrix_size * 3 - 2
        allocate(matrix%val(val_num), matrix%col_ind(val_num), matrix%row_ptr(matrix_size + 1))

        j = 1
        do i = 1, matrix_size
            matrix%row_ptr(i) = j
            if (i > 1) then
                matrix%val(j) = gamma
                matrix%col_ind(j) = i - 1
                j = j + 1
            end if
            matrix%val(j) = 2
            matrix%col_ind(j) = i
            j = j + 1
            if (i < matrix_size) then
                matrix%val(j) = 1
                matrix%col_ind(j) = i + 1
                j = j + 1
            end if
        end do
        matrix%row_ptr(matrix_size + 1) = j
    end subroutine init
end program main
