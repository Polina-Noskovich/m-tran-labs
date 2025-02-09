program lexical_analyzer
  implicit none

  ! Ключевые слова PHP
  character(len=10), dimension(12) :: keywords = (/ 'if      ', 'else    ', 'while   ', 'return  ', &
                                                  'int     ', 'float   ', 'string  ', 'bool    ', &
                                                  'function', 'class   ', 'echo    ', 'for     ' /)

  ! Операторы PHP (унифицированная длина 2 символа)
  character(len=2), dimension(14) :: operators = (/ '= ', '==', '+ ', '- ', '* ', '/ ', '( ', ') ', '{ ', '} ', '; ', '< ', '> ', '? ' /)

  ! Теги PHP
  character(len=2), dimension(2) :: tags = (/ '<?', '?>' /)

  ! Исходная строка программы
  character(len=200) :: line
  integer :: i, len_line, line_number, io_status
  character(len=1) :: ch

  ! Таблица имен
  type :: name_table_entry
     character(len=20) :: name
     character(len=30) :: info
  end type name_table_entry

  type(name_table_entry), dimension(100) :: name_table
  integer :: name_table_size = 0

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

        select case (ch)
        case ('A':'Z', 'a':'z', '$')
           call process_identifier(line, i, len_line, name_table, name_table_size, line_number, keywords)
        case ('0':'9')
           call process_constant(line, i, len_line, name_table, name_table_size, line_number)
        case ('=', '+', '-', '*', '/', '(', ')', '{', '}', ';', '<', '>', '?')
            ! Обработка тегов
            if (i < len_line .and. line(i:i+1) == '<?') then
               write(*, '(A)', advance='no') '<TAG_<?>'
               i = i + 2
            else if (i < len_line .and. line(i:i+1) == '?>') then
               write(*, '(A)', advance='no') '<TAG_?>'
               i = i + 2
            else
               call process_operator(line, i, len_line, line_number, operators)
            end if
        case (' ')
           i = i + 1
        case default
           write(*, '(A,I0,A)', advance='no') '[Error: invalid symbol "' // trim(ch) // '" at line ', line_number
           i = i + 1
        end select
     end do

     write(*, *) ! Переход на новую строку для вывода лексем
  end do

  close(10)

contains

  subroutine process_identifier(line, i, len_line, name_table, name_table_size, line_number, keywords)
    character(len=200), intent(in) :: line
    integer, intent(inout) :: i
    integer, intent(in) :: len_line
    type(name_table_entry), dimension(100), intent(inout) :: name_table
    integer, intent(inout) :: name_table_size
    integer, intent(in) :: line_number
    character(len=10), dimension(10), intent(in) :: keywords

    character(len=20) :: identifier
    character(len=10) :: buffer
    integer :: j

    identifier = ''
    j = 0

    do while (i <= len_line .and. (line(i:i) >= 'A' .and. line(i:i) <= 'Z' .or. &
                                   line(i:i) >= 'a' .and. line(i:i) <= 'z' .or. &
                                   line(i:i) >= '0' .and. line(i:i) <= '9' .or. &
                                   line(i:i) == '$'))
       j = j + 1
       identifier(j:j) = line(i:i)
       i = i + 1
    end do

    identifier = trim(identifier)

    ! Проверка, является ли идентификатор ключевым словом
    do j = 1, size(keywords)
       if (identifier == trim(keywords(j))) then
          write(*, '(A)', advance='no') '<KEY_' // trim(identifier) // '>'
          return
       end if
    end do

    ! Проверка, есть ли идентификатор в таблице
    do j = 1, name_table_size
       if (trim(name_table(j)%name) == identifier) then
          write(buffer, '(I0)') j
          write(*, '(A)', advance='no') '<ID_' // trim(buffer) // '>'
          return
       end if
    end do

    ! Добавление нового идентификатора в таблицу
    name_table_size = name_table_size + 1
    name_table(name_table_size)%name = identifier
    name_table(name_table_size)%info = 'Variable'

    write(buffer, '(I0)') name_table_size
    write(*, '(A)', advance='no') '<ID_' // trim(buffer) // '>'
  end subroutine process_identifier

  subroutine process_constant(line, i, len_line, name_table, name_table_size, line_number)
    character(len=200), intent(in) :: line
    integer, intent(inout) :: i
    integer, intent(in) :: len_line
    type(name_table_entry), dimension(100), intent(inout) :: name_table
    integer, intent(inout) :: name_table_size
    integer, intent(in) :: line_number

    character(len=20) :: constant
    character(len=10) :: buffer
    integer :: j

    constant = ''
    j = 0

    do while (i <= len_line .and. (line(i:i) >= '0' .and. line(i:i) <= '9'))
       j = j + 1
       constant(j:j) = line(i:i)
       i = i + 1
    end do

    constant = trim(constant)

    name_table_size = name_table_size + 1
    name_table(name_table_size)%name = constant
    name_table(name_table_size)%info = 'Constant'

    write(buffer, '(I0)') name_table_size
    write(*, '(A)', advance='no') '<CONST_' // trim(buffer) // '>'
  end subroutine process_constant

subroutine process_operator(line, i, len_line, line_number, operators)
  character(len=200), intent(in) :: line
  integer, intent(inout) :: i
  integer, intent(in) :: len_line
  integer, intent(in) :: line_number
  character(len=2), dimension(13), intent(in) :: operators

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
          write(*, '(A)', advance='no') '<OP_' // trim(op) // '>'
          return
       end if
    end do

    write(*, '(A,I0,A)', advance='no') '[Error: invalid operator "' // trim(op) // '" at line ', line_number
  end subroutine process_operator

  subroutine process_tag(line, i, len_line, line_number, tags)
    character(len=200), intent(in) :: line
    integer, intent(inout) :: i
    integer, intent(in) :: len_line
    integer, intent(in) :: line_number
    character(len=2), dimension(2), intent(in) :: tags

    character(len=2) :: tag
    integer :: j

    if (i < len_line .and. line(i:i+1) == '<?') then
       tag = '<?'
       i = i + 2
    else if (i < len_line .and. line(i:i+1) == '?>') then
       tag = '?>'
       i = i + 2
    else
       i = i + 1
       return
    end if

    ! Проверка на допустимый тег
    do j = 1, size(tags)
       if (tag == trim(tags(j))) then
          write(*, '(A)', advance='no') '<TAG_' // trim(tag) // '>'
          return
       end if
    end do

    write(*, '(A,I0,A)', advance='no') '[Error: invalid tag "' // trim(tag) // '" at line ', line_number
  end subroutine process_tag

end program lexical_analyzer
