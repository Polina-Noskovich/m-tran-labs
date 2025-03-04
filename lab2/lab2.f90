program lexical_analyzer 
implicit none

! Ключевые слова PHP
character(len=10), dimension(16) :: keywords = (/ 'if       ', 'else     ', 'while    ', 'return   ', 'int      ', 'float    ', &
    'string   ', 'bool     ', 'function ', 'class    ', 'echo     ', 'for      ', &
    'final    ', 'abstract ', 'public   ', 'private  ' /)

! Операторы PHP (унифицированная длина 2 символа)
character(len=2), dimension(15) :: operators = (/ '= ', '==', '+ ', '- ', '* ', '/ ', '( ', ') ', '{ ', '} ', '; ', '< ', '> ', '? ', ', ' /)

! Теги PHP
character(len=5), dimension(2) :: tags = (/ '<?php', '?>   ' /)

! Исходная строка программы
character(len=200) :: line
integer :: i, len_line, line_number, io_status
character(len=1) :: ch

! Добавляем флаг для многострочного комментария
logical :: in_comment = .false.

! Таблица имен
type :: name_table_entry
    character(len=20) :: name
    integer :: id
end type name_table_entry

type(name_table_entry), dimension(100) :: name_table
integer :: name_table_size = 0

! Таблица лексем
type :: lexeme_table_entry
    character(len=20) :: lexeme
    integer :: id
end type lexeme_table_entry

type(lexeme_table_entry), dimension(100) :: lexeme_table
integer :: lexeme_table_size = 0

! Таблицы для операторов, идентификаторов, констант и ключевых слов
type(lexeme_table_entry), dimension(100) :: operator_table, identifier_table, constant_table, keyword_table
integer :: operator_table_size = 0, identifier_table_size = 0, constant_table_size = 0, keyword_table_size = 0

! Счетчик токенов
integer :: token_id = 0

! Стек для отслеживания открывающих скобок
integer, dimension(100) :: bracket_stack
integer :: bracket_stack_size = 0

! Открытие файла
open(unit=10, file='INPUT.TXT', status='old', action='read')
line_number = 0

! Чтение файла построчно
do
    read(10, '(A)', iostat=io_status) line
    if (io_status /= 0) exit

    line_number = line_number + 1
    len_line = len_trim(line)
    i = 1

! Анализ строки
do while (i <= len_line)
    ch = line(i:i)

    ! Проверка на многострочный комментарий
    if (in_comment) then
        if (i < len_line .and. line(i:i+1) == '*/') then
            in_comment = .false.
            i = i + 2
        else
            i = i + 1
        end if
        cycle
    end if

    if (i < len_line .and. line(i:i+1) == '//') then
        exit  
    else if (i < len_line .and. line(i:i+1) == '/*') then
        in_comment = .true.
        i = i + 2
        cycle
    end if

        select case (ch)
        case ('A':'Z', 'a':'z', '$')
            call process_identifier(line, i, len_line, name_table, name_table_size, line_number, keywords)
        case ('0':'9')
            call process_constant(line, i, len_line, name_table, name_table_size, line_number)
        case ('=', '+', '-', '*', '/', '(', ')', '{', '}', ';', '<', '>', '?', ',')

            ! Обработка тегов
            if (i < len_line .and. line(i:i+4) == '<?php') then
                call print_token('TAG', '<?php', line_number, i, '-')
                i = i + 5
            else if (i < len_line .and. line(i:i+1) == '?>') then
                call print_token('TAG', '?>', line_number, i, '-')
                i = i + 2
            else
                call process_operator(line, i, len_line, line_number, operators)
            end if
        case ('"', '''')
            call process_string(line, i, len_line, name_table, name_table_size, line_number)
        case (' ')
            i = i + 1
        case default
            call print_token('ERROR', 'Invalid symbol: ' // ch, line_number, i, '-')
            i = i + 1
        end select
    end do

    ! Проверка на незакрытые скобки в конце строки
    if (bracket_stack_size > 0) then
        call print_token('ERROR', 'Unclosed bracket at line ', line_number, bracket_stack(bracket_stack_size), '-')
        bracket_stack_size = 0
    end if
end do

! Проверка на незакрытый многострочный комментарий в конце файла
if (in_comment) then
    write(*, '(A)') 'ERROR: Unclosed multiline comment at end of file.'
end if


close(10)

! Вывод таблиц
call print_table('Operators', operator_table, operator_table_size)
call print_table('Identifiers', identifier_table, identifier_table_size)
call print_table('Constants', constant_table, constant_table_size)
call print_table('Keywords', keyword_table, keyword_table_size)


contains

subroutine print_token(token_type, lexeme, line_num, column, info)
    character(len=*), intent(in) :: token_type, lexeme, info
    integer, intent(in) :: line_num, column
    integer :: token_id_local, j

    ! Проверка, есть ли лексема в таблице
    token_id_local = -1
    if (token_type == 'TAG') then
        token_id_local = 0  ! Устанавливаем фиксированный ID для тегов
    else
        do j = 1, lexeme_table_size
            if (trim(lexeme_table(j)%lexeme) == trim(lexeme)) then
                token_id_local = lexeme_table(j)%id
                exit
            end if
        end do

        if (token_id_local == -1) then
            ! Если лексема новая, добавляем ее в таблицу
            lexeme_table_size = lexeme_table_size + 1
            lexeme_table(lexeme_table_size)%lexeme = trim(lexeme)
            lexeme_table(lexeme_table_size)%id = lexeme_table_size
            token_id_local = lexeme_table_size
        end if
    end if

    ! Заполняем соответствующую таблицу
    select case (token_type)
    case ('OP')
        call add_to_table(operator_table, operator_table_size, lexeme)
    case ('ID')
        call add_to_table(identifier_table, identifier_table_size, lexeme)
    case ('CONST')
        call add_to_table(constant_table, constant_table_size, lexeme)
    case ('KEY')
        call add_to_table(keyword_table, keyword_table_size, lexeme)
    end select

    ! Форматированный вывод с фиксированной шириной столбцов
    write(*, '(A, A12, A, A20, A, I4, A, I4, A, I4, A, A)') &
        'Token: ', trim(token_type), &
        '    Lexeme: ', trim(lexeme), &
        '    Line: ', line_num, &
        ', Column: ', column, &
        ', ID: ', token_id_local, &
        ', Info: ', trim(info)
end subroutine print_token

subroutine add_to_table(table, table_size, lexeme)
    type(lexeme_table_entry), dimension(:), intent(inout) :: table
    integer, intent(inout) :: table_size
    character(len=*), intent(in) :: lexeme
    integer :: j

    do j = 1, table_size
        if (trim(table(j)%lexeme) == trim(lexeme)) return
    end do

    table_size = table_size + 1
    table(table_size)%lexeme = trim(lexeme)
    table(table_size)%id = table_size
end subroutine add_to_table

subroutine print_table(title, table, table_size)
    character(len=*), intent(in) :: title
    type(lexeme_table_entry), dimension(:), intent(in) :: table
    integer, intent(in) :: table_size
    integer :: j

    write(*, '(A)') title
    write(*, '(A, T15, A)') 'Lexeme', 'ID'
    do j = 1, table_size
        write(*, '(A, T15, I4)') trim(table(j)%lexeme), table(j)%id
    end do
    write(*, '(A)') ''
end subroutine print_table

subroutine process_identifier(line, i, len_line, name_table, name_table_size, line_number, keywords)
    character(len=200), intent(in) :: line
    integer, intent(inout) :: i
    integer, intent(in) :: len_line
    type(name_table_entry), dimension(100), intent(inout) :: name_table
    integer, intent(inout) :: name_table_size
    integer, intent(in) :: line_number
    character(len=10), dimension(12), intent(in) :: keywords

    character(len=20) :: identifier
    character(len=10) :: buffer
    integer :: j, id
    character(len=10) :: info_type


    identifier = ''
    j = 0

    ! Собираем идентификатор
    do while (i <= len_line .and. (line(i:i) >= 'A' .and. line(i:i) <= 'Z' .or. &
                                   line(i:i) >= 'a' .and. line(i:i) <= 'z' .or. &
                                   line(i:i) >= '0' .and. line(i:i) <= '9' .or. &
                                   line(i:i) == '$'))
        j = j + 1
        identifier(j:j) = line(i:i)
        i = i + 1
    end do

    identifier = trim(identifier)

    ! Проверка на недопустимые идентификаторы
    if (len_trim(identifier) > 0) then
        ! Случай 1: Идентификатор начинается с цифры (без $)
        if (identifier(1:1) >= '0' .and. identifier(1:1) <= '9') then
            call print_token('ERROR', 'Invalid identifier '//identifier, line_number, i-len_trim(identifier), '-')
            return

        ! Случай 2: Идентификатор начинается с $, за которым следует цифра
        else if (identifier(1:1) == '$' .and. len_trim(identifier) >= 2) then
            if (identifier(2:2) >= '0' .and. identifier(2:2) <= '9') then
                call print_token('ERROR', 'Invalid identifier '//identifier, line_number, i-len_trim(identifier), '-')
                return
            end if
        end if
    end if

    ! Проверка, является ли идентификатор ключевым словом
do j = 1, size(keywords)
    if (identifier == trim(keywords(j))) then
        select case (trim(keywords(j)))
            case ('final', 'abstract', 'public', 'private')  ! <--- Soft keywords
               info_type = 'soft_keyword'
            case ('int')
                info_type = 'integer'
            case ('float')
                info_type = 'floating'
            case ('bool')
                info_type = 'boolean'
            case ('string')
                info_type = 'string'
            case default
                info_type = 'keyword'
        end select
        call print_token('KEY', identifier, line_number, i - len_trim(identifier), info_type)
        return
    end if
end do

! Обработка 'true' и 'false'
if (identifier == 'true' .or. identifier == 'false') then
    call print_token('CONST', identifier, line_number, i - len_trim(identifier), 'boolean')
    return
end if


    ! Проверка, есть ли идентификатор в таблице
    id = -1
    do j = 1, name_table_size
        if (trim(name_table(j)%name) == identifier) then
            id = name_table(j)%id
            exit
        end if
    end do

    if (id == -1) then
        ! Если идентификатор новый, добавляем его в таблицу
        name_table_size = name_table_size + 1
        name_table(name_table_size)%name = identifier
        name_table(name_table_size)%id = name_table_size
        id = name_table_size
    end if

    ! Выводим токен с соответствующим ID
    write(buffer, '(I0)') id
    call print_token('ID', identifier, line_number, i - len_trim(identifier), '-')
end subroutine process_identifier

subroutine process_constant(line, i, len_line, name_table, name_table_size, line_number)
    character(len=200), intent(in) :: line
    integer, intent(inout) :: i
    integer, intent(in) :: len_line
    type(name_table_entry), dimension(100), intent(inout) :: name_table
    integer, intent(inout) :: name_table_size
    integer, intent(in) :: line_number

    character(len=200) :: constant
    character(len=10) :: buffer
    integer :: j
    logical :: has_dot
    character(len=10) :: info_type


    constant = ''
    j = 0
    has_dot = .false.

    do while (i <= len_line .and. (line(i:i) >= '0' .and. line(i:i) <= '9' .or. line(i:i) == '.'))
        if (line(i:i) == '.' .and. has_dot) exit
        if (line(i:i) == '.') has_dot = .true.
        j = j + 1
        constant(j:j) = line(i:i)
        i = i + 1
    end do

    if (has_dot) then
    info_type = 'floating'
else
    info_type = 'integer'
end if

    constant = trim(constant)

    ! Заполняем таблицу символов
    name_table_size = name_table_size + 1
    name_table(name_table_size)%name = constant
    name_table(name_table_size)%id = name_table_size

    write(buffer, '(I0)') name_table_size

call print_token('CONST', constant, line_number, i - len_trim(constant), info_type)
end subroutine process_constant

subroutine process_string(line, i, len_line, name_table, name_table_size, line_number)
    character(len=200), intent(in) :: line
    integer, intent(inout) :: i
    integer, intent(in) :: len_line
    type(name_table_entry), dimension(100), intent(inout) :: name_table
    integer, intent(inout) :: name_table_size
    integer, intent(in) :: line_number

    character(len=200) :: str
    character(len=10) :: buffer
    integer :: j
    character(len=1) :: quote_char

    str = ''
    j = 0
    quote_char = line(i:i)
    i = i + 1

    do while (i <= len_line .and. line(i:i) /= quote_char)
        j = j + 1
        str(j:j) = line(i:i)
        i = i + 1
    end do

    if (i <= len_line) then
        i = i + 1
    else
        call print_token('ERROR', 'Unterminated string at line ' // trim(str), line_number, i, '-')
        return
    end if

    str = trim(str)

    ! Заполняем таблицу символов
    name_table_size = name_table_size + 1
    name_table(name_table_size)%name = str
    name_table(name_table_size)%id = name_table_size

    write(buffer, '(I0)') name_table_size
call print_token('STR', str, line_number, i - len_trim(str), 'string')
end subroutine process_string

subroutine process_operator(line, i, len_line, line_number, operators)
    character(len=200), intent(in) :: line
    integer, intent(inout) :: i
    integer, intent(in) :: len_line
    integer, intent(in) :: line_number
    character(len=2), dimension(15), intent(in) :: operators

    character(len=2) :: op
    integer :: j

    if (i < len_line .and. line(i:i+1) == '==') then
        op = '=='
        i = i + 1
    else
        op = line(i:i)
    end if
    i = i + 1

    ! Проверка на допустимый оператор
    do j = 1, size(operators)
        if (op == trim(operators(j))) then
            if (op == '(') then
                ! Добавляем открывающую скобку в стек
                bracket_stack_size = bracket_stack_size + 1
                bracket_stack(bracket_stack_size) = i - 1
            else if (op == ')') then
                ! Проверяем, есть ли соответствующая открывающая скобка
                if (bracket_stack_size > 0) then
                    bracket_stack_size = bracket_stack_size - 1
                else
                    call print_token('ERROR', 'Unmatched closing bracket', line_number, i - 1, '-')
                end if
            end if
            call print_token('OP', op, line_number, i - len_trim(op), '-')
            return
        end if
    end do

    call print_token('ERROR', 'Invalid operator: ' // op, line_number, i - len_trim(op), '-')
end subroutine process_operator

end program lexical_analyzer