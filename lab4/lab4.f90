MODULE SymbolTable
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_symbols = 1000
  INTEGER, PARAMETER :: max_functions = 50
  INTEGER, PARAMETER :: max_scope_depth = 10
  INTEGER, PARAMETER :: max_errors = 100  ! Максимальное количество ошибок

  TYPE Symbol
    CHARACTER(LEN=32) :: name
    CHARACTER(LEN=10) :: type  ! 'int', 'float', 'string', 'void'
    CHARACTER(LEN=10) :: scope ! 'global', 'local'
    INTEGER :: scope_level     ! Уровень вложенности области видимости
  END TYPE Symbol

  TYPE Function
    CHARACTER(LEN=32) :: name
    CHARACTER(LEN=10) :: return_type
    INTEGER :: arg_count
    CHARACTER(LEN=10), DIMENSION(10) :: arg_types
  END TYPE Function

  TYPE Class
    CHARACTER(LEN=32) :: name
  END TYPE Class

  INTEGER, PARAMETER :: max_classes = 50
  TYPE(Class), DIMENSION(max_classes) :: classes
  INTEGER :: class_count = 0

  TYPE(Symbol), DIMENSION(max_symbols) :: symbols
  TYPE(Function), DIMENSION(max_functions) :: functions
  CHARACTER(LEN=256), DIMENSION(max_errors) :: errors  ! Массив для хранения ошибок
  INTEGER :: symbol_count = 0
  INTEGER :: function_count = 0
  INTEGER :: current_scope_level = 0
  INTEGER :: error_count = 0  ! Счетчик ошибок
  INTEGER :: current_function_index = 0  ! Индекс текущей функции в массиве functions
  LOGICAL :: expected_braces = .FALSE.  ! Ожидается ли '{' после объявления функции
  LOGICAL :: inside_function = .FALSE.  ! Флаг для отслеживания нахождения внутри функции
  LOGICAL :: inside_class = .FALSE.     ! Флаг для отслеживания нахождения внутри класса

CONTAINS
  ! Очистка имени переменной от символов '$' и пробелов
  FUNCTION CleanVarName(raw_name) RESULT(clean_name)
    CHARACTER(LEN=*), INTENT(IN) :: raw_name
    CHARACTER(LEN=32) :: clean_name
    INTEGER :: i, j
    
    clean_name = ADJUSTL(raw_name)  ! Удаляем начальные пробелы
    
    ! Удаляем символ '$', если он есть
    IF (clean_name(1:1) == '$') THEN
      clean_name = clean_name(2:)
      clean_name = ADJUSTL(clean_name)
    END IF
    
    ! Удаляем завершающие символы ';' и пробелы
    j = LEN_TRIM(clean_name)
    DO WHILE (j > 0 .AND. (clean_name(j:j) == ';' .OR. clean_name(j:j) == ' '))
      j = j - 1
    END DO
    clean_name = clean_name(1:j)
  END FUNCTION CleanVarName

  ! Добавление символа в таблицу
  SUBROUTINE AddSymbol(name, type, scope)
    CHARACTER(LEN=*), INTENT(IN) :: name, type, scope
    CHARACTER(LEN=32) :: cleaned_name
    
    cleaned_name = CleanVarName(name)  ! Очищаем имя переменной
    
    IF (symbol_count < max_symbols) THEN
      symbol_count = symbol_count + 1
      symbols(symbol_count)%name = cleaned_name
      symbols(symbol_count)%type = type
      symbols(symbol_count)%scope = scope
      symbols(symbol_count)%scope_level = current_scope_level
      PRINT *, "Added symbol: ", TRIM(cleaned_name), " Type:", TRIM(type), " Scope:", TRIM(scope), " Level:", current_scope_level
    ELSE
      CALL AddError("Error: Symbol table overflow!")
    END IF
  END SUBROUTINE AddSymbol
  
  ! Получение типа символа по имени
  FUNCTION GetSymbolType(name) RESULT(sym_type)
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=10) :: sym_type
    CHARACTER(LEN=32) :: cleaned_name
    INTEGER :: i
    
    cleaned_name = CleanVarName(name)  ! Очищаем имя переменной
    sym_type = "undefined"
    
    DO i = 1, symbol_count
      IF (TRIM(symbols(i)%name) == TRIM(cleaned_name)) THEN
        ! Глобальные переменные доступны только на уровне 0
        IF (symbols(i)%scope == "global" .AND. current_scope_level == 0) THEN
          sym_type = symbols(i)%type
          RETURN
        END IF
        ! Локальные переменные доступны на текущем уровне или выше
        IF (symbols(i)%scope == "local" .AND. symbols(i)%scope_level <= current_scope_level) THEN
          sym_type = symbols(i)%type
          RETURN
        END IF
      END IF
    END DO
  END FUNCTION GetSymbolType

  ! Проверка, является ли строка числом
  FUNCTION IsNumber(str) RESULT(is_num)
    CHARACTER(LEN=*), INTENT(IN) :: str
    LOGICAL :: is_num
    INTEGER :: i, dot_count
    
    is_num = .TRUE.
    dot_count = 0
    
    DO i = 1, LEN_TRIM(str)
      IF (str(i:i) == '.') THEN
        dot_count = dot_count + 1
        IF (dot_count > 1) THEN
          is_num = .FALSE.
          RETURN
        END IF
      ELSE IF (str(i:i) < '0' .OR. str(i:i) > '9') THEN
        is_num = .FALSE.
        RETURN
      END IF
    END DO
  END FUNCTION IsNumber

  ! Удаление комментариев из строки
  FUNCTION RemoveComments(line) RESULT(clean_line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=256) :: clean_line
    INTEGER :: pos
    
    clean_line = line
    pos = INDEX(clean_line, "//")
    IF (pos > 0) clean_line = clean_line(:pos-1)
    pos = INDEX(clean_line, "#")
    IF (pos > 0) clean_line = clean_line(:pos-1)
    pos = INDEX(clean_line, "/*")
    IF (pos > 0) clean_line = clean_line(:pos-1)
  END FUNCTION RemoveComments

  ! Проверка синтаксиса: наличие точки с запятой в конце строки
  FUNCTION CheckSyntax(line) RESULT(is_valid)
    CHARACTER(LEN=*), INTENT(IN) :: line
    LOGICAL :: is_valid
    INTEGER :: len_line
    
    len_line = LEN_TRIM(line)
    is_valid = .TRUE.
    
    ! Игнорируем PHP-теги, пустые строки и фигурные скобки
    IF (len_line == 0 .OR. INDEX(line, "<?php") > 0 .OR. &
        TRIM(line) == "?>" .OR. INDEX(line, "{") > 0 .OR. INDEX(line, "}") > 0) RETURN
    
    ! Проверка точки с запятой
    IF (line(len_line:len_line) /= ';') THEN
      CALL AddError("Syntax error: Missing semicolon at the end of line: ["//TRIM(line)//"]")
      is_valid = .FALSE.
    END IF
  END FUNCTION CheckSyntax

  ! Обработка присваивания
  SUBROUTINE ProcessAssignment(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: var_name, var_type, expr
    INTEGER :: eq_pos, quote_pos, dot_pos

    ! Игнорируем PHP-теги
    IF (INDEX(line, "<?php") > 0 .OR. TRIM(line) == "?>") RETURN  

    PRINT *, "Processing assignment: [", TRIM(line), "]"
    eq_pos = INDEX(line, "=")
    IF (eq_pos > 0) THEN
      var_name = ADJUSTL(line(:eq_pos-1))
      expr = ADJUSTL(line(eq_pos+1:))
      quote_pos = INDEX(expr, '"')
      dot_pos = INDEX(expr, '.')
      
      ! Определяем тип
      IF (quote_pos > 0) THEN
        var_type = "string"
      ELSE IF (dot_pos > 0) THEN
        var_type = "string"  ! Если есть оператор конкатенации, тип всегда string
      ELSE
        var_type = "int"
      END IF

      ! Добавляем переменную в таблицу символов
      IF (current_scope_level == 0) THEN
        CALL AddSymbol(var_name, var_type, "global")
      ELSE
        CALL AddSymbol(var_name, var_type, "local")
      END IF
      
      ! Если есть операция (+, -, *, /, .), обрабатываем выражение
      IF (SCAN(expr, "+-*/.") > 0) THEN
        CALL ProcessExpression(expr)
      END IF
    END IF
  END SUBROUTINE ProcessAssignment
  
  ! Обработка выражения
  SUBROUTINE ProcessExpression(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: var1, var2
    CHARACTER(LEN=10) :: type1, type2
    INTEGER :: op_pos
    
    PRINT *, "Processing expression: [", TRIM(line), "]"
    
    ! Находим первый оператор
    op_pos = SCAN(line, "+-*/.")
    IF (op_pos == 0) RETURN
    
    ! Разделяем левую и правую части
    var1 = CleanVarName(line(1:op_pos-1))
    var2 = CleanVarName(line(op_pos+1:))
    
    ! Получаем типы переменных
    IF (IsNumber(var1)) THEN
      type1 = "int"
    ELSE IF (var1(1:1) == '"' .AND. var1(LEN_TRIM(var1):LEN_TRIM(var1)) == '"') THEN
      type1 = "string"
    ELSE
      type1 = GetSymbolType(var1)
    END IF
    
    IF (IsNumber(var2)) THEN
      type2 = "int"
    ELSE IF (var2(1:1) == '"' .AND. var2(LEN_TRIM(var2):LEN_TRIM(var2)) == '"') THEN
      type2 = "string"
    ELSE
      type2 = GetSymbolType(var2)
    END IF
    
    PRINT *, "Variables: '", TRIM(var1), "' and '", TRIM(var2), "'"
    PRINT *, "Types: ", TRIM(type1), " and ", TRIM(type2)
    
    IF (type1 == "undefined" .OR. type2 == "undefined") THEN
      CALL AddError("ERROR: Undefined variable in expression: "//TRIM(line))
    ELSE IF (line(op_pos:op_pos) == '.' .AND. (type1 == "string" .OR. type2 == "string")) THEN
      PRINT *, "Valid concatenation: ", TRIM(line)
    ELSE IF (type1 /= type2) THEN
      CALL AddError("ERROR: Type mismatch in expression: "//TRIM(line))
    ELSE
      PRINT *, "Valid operation: ", TRIM(line)
    END IF

    ! Проверка доступности переменных
    IF (GetSymbolType(var1) == "undefined") THEN
      CALL AddError("ERROR: Undefined variable '"//TRIM(var1)//"'")
    END IF
    IF (GetSymbolType(var2) == "undefined") THEN
      CALL AddError("ERROR: Undefined variable '"//TRIM(var2)//"'")
    END IF
  END SUBROUTINE ProcessExpression

  ! Обработка объявления функции
  SUBROUTINE ProcessFunctionDeclaration(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: func_name
    INTEGER :: pos1, pos2, pos3, pos_brace, i, start, end
    CHARACTER(LEN=32) :: arg_name

    PRINT *, "Processing function declaration: [", TRIM(line), "]"
    pos1 = INDEX(line, "function")
    pos2 = INDEX(line, "(")
    pos3 = INDEX(line, ")")
    pos_brace = INDEX(line, "{")
    
    IF (pos1 > 0 .AND. pos2 > 0 .AND. pos3 > 0) THEN
      func_name = ADJUSTL(line(pos1+8:pos2-1))
      
      ! Проверка на повторное объявление функции
      DO i = 1, function_count
        IF (TRIM(functions(i)%name) == TRIM(func_name)) THEN
          CALL AddError("Error: Function '"//TRIM(func_name)//"' is already declared!")
          RETURN
        END IF
      END DO

      ! Добавляем функцию в таблицу функций
      IF (function_count < max_functions) THEN
        function_count = function_count + 1
        functions(function_count)%name = func_name
        functions(function_count)%return_type = "void"
        functions(function_count)%arg_count = 0
        current_function_index = function_count
        PRINT *, "Added function: ", TRIM(func_name), " Return type: void"
      ELSE
        CALL AddError("Error: Function table overflow!")
      END IF

      ! Обрабатываем аргументы функции
      start = pos2 + 1
      DO WHILE (start < pos3)
        end = INDEX(line(start:pos3), ',')
        IF (end == 0) end = pos3 - start + 1
        arg_name = ADJUSTL(line(start:start + end - 2))
        arg_name = CleanVarName(arg_name)
        CALL AddSymbol(arg_name, "int", "local")
        functions(function_count)%arg_count = functions(function_count)%arg_count + 1
        PRINT *, "Added argument: ", TRIM(arg_name), " Type: int Scope: local"
        start = start + end
      END DO

      ! Проверяем, есть ли открывающая скобка в этой строке
      IF (pos_brace > 0) THEN
        current_scope_level = current_scope_level + 1
        expected_braces = .FALSE.
        inside_function = .TRUE.
        PRINT *, "Entered function body for ", TRIM(func_name)
      ELSE
        expected_braces = .TRUE.
      END IF
    END IF
  END SUBROUTINE ProcessFunctionDeclaration

  ! Обработка объявления класса
  SUBROUTINE ProcessClassDeclaration(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: class_name
    INTEGER :: pos1, pos2, i, end_pos

    PRINT *, "Processing class declaration: [", TRIM(line), "]"
    pos1 = INDEX(line, "class")
    IF (pos1 > 0) THEN
      pos1 = pos1 + 5  ! Пропускаем ключевое слово "class"
      pos2 = INDEX(line(pos1:), " ")  ! Ищем первый пробел после "class"
      IF (pos2 == 0) THEN
        CALL AddError("Error: Invalid class declaration syntax.")
        RETURN
      END IF
      pos2 = pos1 + pos2 - 1
      class_name = ADJUSTL(line(pos2+1:))
      end_pos = INDEX(class_name, " ")  ! Конец имени класса
      IF (end_pos == 0) end_pos = LEN_TRIM(class_name) + 1
      class_name = class_name(1:end_pos-1)
      class_name = CleanVarName(class_name)

      ! Проверка на повторное объявление класса
      DO i = 1, class_count
        IF (TRIM(classes(i)%name) == TRIM(class_name)) THEN
          CALL AddError("Error: Class '"//TRIM(class_name)//"' is already declared!")
          RETURN
        END IF
      END DO

      IF (class_count < max_classes) THEN
        class_count = class_count + 1
        classes(class_count)%name = class_name
        PRINT *, "Added class: ", TRIM(class_name)
      ELSE
        CALL AddError("Error: Class table overflow!")
      END IF
    END IF
  END SUBROUTINE ProcessClassDeclaration

  ! Подсчет количества аргументов в вызове функции
  SUBROUTINE CountArguments(args_str, count)
    CHARACTER(LEN=*), INTENT(IN) :: args_str
    INTEGER, INTENT(OUT) :: count
    INTEGER :: i, len_str

    count = 0
    len_str = LEN_TRIM(args_str)
    IF (len_str == 0) RETURN

    count = 1
    DO i = 1, len_str
      IF (args_str(i:i) == ',') THEN
        count = count + 1
      END IF
    END DO
  END SUBROUTINE CountArguments

  ! Преобразование целого числа в строку
  FUNCTION INT_TO_STR(i) RESULT(s)
    INTEGER, INTENT(IN) :: i
    CHARACTER(LEN=20) :: s
    WRITE(s, '(I10)') i
    s = ADJUSTL(s)
  END FUNCTION INT_TO_STR

  ! Определение типа выражения
  RECURSIVE FUNCTION DetermineExpressionType(expr) RESULT(expr_type)
    CHARACTER(LEN=*), INTENT(IN) :: expr
    CHARACTER(LEN=10) :: expr_type
    CHARACTER(LEN=32) :: var1, var2
    CHARACTER(LEN=10) :: type1, type2
    INTEGER :: op_pos, len_expr

    expr_type = "undefined"
    len_expr = LEN_TRIM(expr)
    IF (len_expr == 0) RETURN

    IF (IsNumber(expr)) THEN
      expr_type = "int"
      RETURN
    END IF

    IF (expr(1:1) == '"' .AND. expr(len_expr:len_expr) == '"') THEN
      expr_type = "string"
      RETURN
    END IF

    op_pos = SCAN(expr, "+-*/.")
    IF (op_pos == 0) THEN
      expr_type = GetSymbolType(expr)
    ELSE
      var1 = CleanVarName(expr(1:op_pos-1))
      var2 = CleanVarName(expr(op_pos+1:))

      type1 = DetermineExpressionType(var1)
      type2 = DetermineExpressionType(var2)

      IF (type1 == "undefined" .OR. type2 == "undefined") THEN
        expr_type = "undefined"
      ELSE IF (expr(op_pos:op_pos) == '.' .AND. (type1 == "string" .OR. type2 == "string")) THEN
        expr_type = "string"
      ELSE IF (type1 == type2) THEN
        expr_type = type1
      ELSE
        expr_type = "undefined"
      END IF
    END IF
  END FUNCTION DetermineExpressionType

  ! Обработка вызова функции
  SUBROUTINE ProcessFunctionCall(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: func_name
    INTEGER :: pos1, pos2, eq_pos, func_index, passed_args, declared_args
    CHARACTER(LEN=256) :: args_part

    PRINT *, "Processing function call: [", TRIM(line), "]"
    pos1 = INDEX(line, "(")
    pos2 = INDEX(line, ")")
    
    IF (pos1 > 0 .AND. pos2 > 0) THEN
      eq_pos = INDEX(line, "=")
      IF (eq_pos > 0) THEN
        func_name = ADJUSTL(line(eq_pos+1:pos1-1))
      ELSE
        func_name = ADJUSTL(line(:pos1-1))
      END IF
      func_name = CleanVarName(func_name)
      
      func_index = GetFunctionIndex(func_name)
      IF (func_index == 0) THEN
        CALL AddError("Error: Function '"//TRIM(func_name)//"' is not declared!")
      ELSE
        args_part = line(pos1+1:pos2-1)
        CALL CountArguments(ADJUSTL(args_part), passed_args)
        declared_args = functions(func_index)%arg_count
        IF (passed_args /= declared_args) THEN
          CALL AddError("Error: Function '"//TRIM(func_name)//"' expects "// &
                        TRIM(INT_TO_STR(declared_args))//" arguments, but "// &
                        TRIM(INT_TO_STR(passed_args))//" were passed.")
        END IF
        PRINT *, "Valid function call: ", TRIM(func_name)
      END IF
    END IF
  END SUBROUTINE ProcessFunctionCall

  ! Поиск индекса функции в таблице функций
  FUNCTION GetFunctionIndex(name) RESULT(index)
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER :: index, i
    
    index = 0
    DO i = 1, function_count
      IF (TRIM(functions(i)%name) == TRIM(name)) THEN
        index = i
        RETURN
      END IF
    END DO
  END FUNCTION GetFunctionIndex

  SUBROUTINE ProcessCodeBlock(lines, num_lines)
    CHARACTER(LEN=256), DIMENSION(:), INTENT(IN) :: lines
    INTEGER, INTENT(IN) :: num_lines
    INTEGER :: i

    DO i = 1, num_lines
      CALL ProcessLine(lines(i))
    END DO
  END SUBROUTINE ProcessCodeBlock
  
  ! Обработка одной строки
  SUBROUTINE ProcessLine(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=256) :: clean_line
    INTEGER :: i, pos_brace
    CHARACTER(LEN=256) :: expr
    CHARACTER(LEN=10) :: expr_type

    clean_line = RemoveComments(line)
    PRINT *, "Processing line: [", TRIM(clean_line), "]"
    IF (LEN_TRIM(clean_line) == 0) RETURN

    ! Обработка фигурных скобок для управления областью видимости
    IF (expected_braces) THEN
      pos_brace = INDEX(clean_line, '{')
      IF (pos_brace > 0) THEN
        current_scope_level = current_scope_level + 1
        expected_braces = .FALSE.
        PRINT *, "Entered function body for ", TRIM(functions(current_function_index)%name)
      END IF
    END IF

    DO i = 1, LEN_TRIM(clean_line)
      IF (clean_line(i:i) == '{') THEN
        IF (inside_function .OR. inside_class) THEN
          current_scope_level = current_scope_level + 1
        END IF
      ELSE IF (clean_line(i:i) == '}') THEN
        IF (inside_function .OR. inside_class) THEN
          current_scope_level = current_scope_level - 1
          IF (current_scope_level == 0) THEN
            inside_function = .FALSE.
            inside_class = .FALSE.
            current_function_index = 0  ! Выход из глобальной области
          END IF
        END IF
      END IF
    END DO

    ! Обработка return
    IF (INDEX(clean_line, "return") > 0) THEN
      PRINT *, "Processing return statement: [", TRIM(clean_line), "]"
      expr = ADJUSTL(clean_line(INDEX(clean_line, "return") + 6:))
      expr_type = DetermineExpressionType(TRIM(expr))
      IF (current_function_index > 0) THEN
        IF (functions(current_function_index)%return_type == "void") THEN
          functions(current_function_index)%return_type = expr_type
          PRINT *, "Function ", TRIM(functions(current_function_index)%name), " return type set to ", TRIM(expr_type)
        ELSE IF (functions(current_function_index)%return_type /= expr_type) THEN
          CALL AddError("Return type mismatch in function "//TRIM(functions(current_function_index)%name)//&
                        ": Expected "//TRIM(functions(current_function_index)%return_type)//&
                        ", got "//TRIM(expr_type))
        END IF
      ELSE
        CALL AddError("Return statement outside of any function.")
      END IF
      RETURN
    END IF

    ! Игнорируем echo
    IF (INDEX(clean_line, "echo") > 0) THEN
      PRINT *, "Ignoring echo statement: [", TRIM(clean_line), "]"
      RETURN
    END IF

    ! Проверка синтаксиса
    IF (.NOT. CheckSyntax(clean_line)) THEN
      PRINT *, "Syntax error detected. Skipping line."
      RETURN
    END IF

    ! Определение типа строки и обработка
    IF (INDEX(clean_line, "class") > 0) THEN
      CALL ProcessClassDeclaration(clean_line)
      inside_class = .TRUE.
    ELSE IF (INDEX(clean_line, "function") > 0) THEN
      CALL ProcessFunctionDeclaration(clean_line)
      inside_function = .TRUE.
    ELSE IF (INDEX(clean_line, "(") > 0 .AND. INDEX(clean_line, ")") > 0) THEN
      CALL ProcessFunctionCall(clean_line)
    ELSE IF (INDEX(clean_line, "=") > 0) THEN
      CALL ProcessAssignment(clean_line)
    ELSE
      CALL ProcessExpression(clean_line)
    END IF
  END SUBROUTINE ProcessLine

  ! Добавление ошибки в список
  SUBROUTINE AddError(error_message)
    CHARACTER(LEN=*), INTENT(IN) :: error_message
    IF (error_count < max_errors) THEN
      error_count = error_count + 1
      errors(error_count) = error_message
    ELSE
      PRINT *, "Error: Too many errors! Cannot add more."
    END IF
  END SUBROUTINE AddError

  ! Вывод таблицы символов
  SUBROUTINE PrintSymbolTable()
    INTEGER :: i
    CHARACTER(LEN=50) :: fmt  ! Форматная строка

    ! Формат для заголовков
    fmt = "(A20, A10, A10, A10)"

    PRINT *, "Symbol Table:"
    PRINT *, "--------------------------------------------------"
    PRINT *, "     Name      Type     Scope      Level"
    PRINT *, "--------------------------------------------------"

    ! Формат для данных
    fmt = "(A10, A10, A10, I10)"

    DO i = 1, symbol_count
      WRITE(*, fmt) TRIM(symbols(i)%name), TRIM(symbols(i)%type), TRIM(symbols(i)%scope), symbols(i)%scope_level
    END DO

    PRINT *, "--------------------------------------------------"
  END SUBROUTINE PrintSymbolTable

  ! Вывод списка ошибок
  SUBROUTINE PrintErrors()
    INTEGER :: i

    IF (error_count > 0) THEN
      PRINT *, "Errors:"
      PRINT *, "---------------------------------------------"
      DO i = 1, error_count
        PRINT *, TRIM(errors(i))
      END DO
      PRINT *, "---------------------------------------------"
    ELSE
      PRINT *, "Semantic analysis completed successfully!"
    END IF
  END SUBROUTINE PrintErrors

END MODULE SymbolTable

PROGRAM SemanticAnalyzer
  USE SymbolTable
  IMPLICIT NONE
  CHARACTER(LEN=256) :: line
  CHARACTER(LEN=256), DIMENSION(1000) :: lines
  INTEGER :: num_lines = 0, ios

  OPEN(UNIT=10, FILE='INPUT.TXT', STATUS='OLD', ACTION='READ', IOSTAT=ios)
  IF (ios /= 0) THEN
    PRINT *, "Error: Unable to open file INPUT.TXT"
    STOP
  END IF
  
  PRINT *, "File opened successfully!"

  ! Чтение всех строк из файла
  DO
    READ(10, '(A)', IOSTAT=ios) line
    IF (ios /= 0) EXIT
    num_lines = num_lines + 1
    lines(num_lines) = line
  END DO
  
  CLOSE(10)

  ! Обработка всех строк
  CALL ProcessCodeBlock(lines, num_lines)

  ! Вывод таблицы символов
  CALL PrintSymbolTable()

  ! Вывод ошибок (если есть)
  CALL PrintErrors()

END PROGRAM SemanticAnalyzer