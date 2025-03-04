program php_parser
    implicit none
    character(len=1000) :: line
    integer :: iostat

    ! Открываем файл INPUT.TXT
    open(unit=10, file="INPUT.TXT", status="old", action="read", iostat=iostat)
    if (iostat /= 0) then
        print *, "Ошибка: не удалось открыть файл INPUT.TXT"
        stop
    end if

    print *, "+-- PHP_OPEN_TAG (<?php)"

    ! Читаем файл построчно
    do
        read(10, '(A)', iostat=iostat) line
        if (iostat /= 0) exit
        call process_line(trim(line))
    end do

    print *, "+-- PHP_CLOSE_TAG (?>)"

    close(10)

contains

    subroutine process_line(line)
        character(len=*) :: line
        character(len=100) :: token

        if (index(line, "echo") /= 0) then
            print *, "|   +-- ExpressionStatement"
            print *, "|   |   +-- EchoStatement"
            call extract_string(line, token)
            print *, "|   |   |   +-- StringLiteral (", trim(token), ")"
        else if (index(line, "=") /= 0 .and. index(line, "echo") == 0 .and. index(line, "for") == 0) then
            print *, "|   +-- ExpressionStatement"
            print *, "|   |   +-- Assignment"
            call extract_variable(line, token)
            print *, "|   |   |   +-- Variable (", trim(token), ")"
            print *, "|   |   |   +-- Operator (=)"
            call extract_expression(line, token)
        else if (index(line, "if") /= 0) then
            print *, "|   +-- IfStatement"
            call extract_condition(line, token)
            print *, "|   |   +-- Condition (", trim(token), ")"
        else if (index(line, "else") /= 0) then
            print *, "|   +-- ElseStatement"
        else if (index(line, "for") /= 0) then
            print *, "|   +-- ForLoop"
            call extract_for_loop(line)
        else if (index(line, "while") /= 0) then
            print *, "|   +-- WhileLoop"
        else if (index(line, "function") /= 0) then
            print *, "|   +-- FunctionDefinition"
            call extract_function(line, token)
            print *, "|   |   +-- FunctionName (", trim(token), ")"
    endif
    end subroutine process_line

    subroutine extract_string(line, token)
        character(len=*) :: line
        character(len=100) :: token
        integer :: start, stop

        start = index(line, '"')
        stop = index(line(start+1:), '"') + start
        if (start > 0 .and. stop > start) then
            token = line(start:stop)
        else
            token = "Error"
        end if
    end subroutine extract_string

    subroutine extract_variable(line, token)
        character(len=*) :: line
        character(len=100) :: token
        integer :: pos

        pos = index(line, "$")
        if (pos > 0) then
            token = adjustl(line(pos:index(line, "=")-1))
        else
            token = "Error"
        end if
    end subroutine extract_variable

    subroutine extract_expression(line, token)
        character(len=*) :: line
        character(len=100) :: token
        integer :: pos

        pos = index(line, "=") + 1
        if (pos > 1) then
            token = adjustl(line(pos:))
            print *, "|   |   |   +-- Expression (", trim(token), ")"
        else
            token = "Error"
        end if
    end subroutine extract_expression

    subroutine extract_condition(line, token)
        character(len=*) :: line
        character(len=100) :: token
        integer :: start, stop

        start = index(line, "(")
        stop = index(line, ")")
        if (start > 0 .and. stop > start) then
            token = line(start+1:stop-1)
        else
            token = "Error"
        end if
    end subroutine extract_condition

    subroutine extract_for_loop(line)
        character(len=*) :: line
        character(len=100) :: token
        integer :: start, stop

        start = index(line, "(")
        stop = index(line, ")")
        if (start > 0 .and. stop > start) then
            token = line(start+1:stop-1)
            print *, "|   |   +-- LoopCondition (", trim(token), ")"
        else
            print *, "|   |   +-- Error: неверный синтаксис цикла for"
        end if
    end subroutine extract_for_loop

    subroutine extract_function(line, token)
        character(len=*) :: line
        character(len=100) :: token
        integer :: start, stop

        start = index(line, "function") + 9
        stop = index(line, "(") - 1
        if (start > 0 .and. stop > start) then
            token = line(start:stop)
        else
            token = "Error"
        end if
    end subroutine extract_function

end program php_parser
