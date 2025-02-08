program example
    implicit none
    ! Числовые и текстовые константы
    real, parameter :: PI = 3.14159
    character(len=20), parameter :: GREETING = "Hello, Fortran!"

    ! Переменные
    integer :: i
    real :: radius, area
    logical :: is_large

    ! Ввод радиуса
    print *, "vvedite radius:"
    read *, radius

    ! Вычисление площади
    area = PI * radius**2

    ! Условный оператор
    if (area > 100.0) then
        is_large = .true.
        print *, "Ploshad big!"
    else
        is_large = .false.
        print *, "mini."
    end if

    ! Цикл
    do i = 1, 5
        print *, "Iteracia:", i
    end do

    ! Вывод результата
    print *, GREETING
    print *, "Ploshad kruga:", area
end program example