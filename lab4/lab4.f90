MODULE SymbolTable
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_symbols = 100
  TYPE Symbol
    CHARACTER(LEN=32) :: name
    CHARACTER(LEN=10) :: type  ! 'int', 'float', 'string'
    CHARACTER(LEN=10) :: scope ! 'global', 'local'
  END TYPE Symbol
  TYPE(Symbol), DIMENSION(max_symbols) :: symbols
  INTEGER :: symbol_count = 0

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
      PRINT *, "Added symbol: ", TRIM(cleaned_name), " Type:", TRIM(type), " Scope:", TRIM(scope)
    ELSE
      PRINT *, "Error: Symbol table overflow!"
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
        sym_type = symbols(i)%type
        RETURN
      END IF
    END DO
  END FUNCTION GetSymbolType

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

  ! Обработка присваивания
  SUBROUTINE ProcessAssignment(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: var_name, var_type, expr
    INTEGER :: eq_pos, quote_pos

    ! Игнорируем PHP-теги
    IF (INDEX(line, "<?php") > 0 .OR. TRIM(line) == "?>") RETURN  

    PRINT *, "Processing assignment: [", TRIM(line), "]"
    eq_pos = INDEX(line, "=")
    IF (eq_pos > 0) THEN
      var_name = ADJUSTL(line(:eq_pos-1))
      expr = ADJUSTL(line(eq_pos+1:))
      quote_pos = INDEX(expr, '"')
      
      ! Проверяем, есть ли операция (+, -, *, /)
      IF (SCAN(expr, "+-*/") > 0) THEN
        CALL ProcessExpression(expr)
        RETURN
      END IF

      ! Определяем тип
      IF (quote_pos > 0) THEN
        var_type = "string"
      ELSE
        var_type = "int"
      END IF

      CALL AddSymbol(var_name, var_type, "global")
    END IF
  END SUBROUTINE ProcessAssignment
  
  ! Обработка выражения
  SUBROUTINE ProcessExpression(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: var1, var2, expr_part
    CHARACTER(LEN=10) :: type1, type2
    INTEGER :: op_pos, i
    
    PRINT *, "Processing expression: [", TRIM(line), "]"
    
    ! Находим первый оператор
    op_pos = SCAN(line, "+-*/")
    IF (op_pos == 0) RETURN
    
    ! Разделяем левую и правую части
    var1 = CleanVarName(line(1:op_pos-1))
    var2 = CleanVarName(line(op_pos+1:))
    
    ! Получаем типы переменных
    type1 = GetSymbolType(var1)
    type2 = GetSymbolType(var2)
    
    PRINT *, "Variables: '", TRIM(var1), "' and '", TRIM(var2), "'"
    PRINT *, "Types: ", TRIM(type1), " and ", TRIM(type2)
    
    IF (type1 == "undefined" .OR. type2 == "undefined") THEN
      PRINT *, "ERROR: Undefined variable!"
    ELSE IF (type1 /= type2) THEN
      PRINT *, "ERROR: Type mismatch!"
    ELSE
      PRINT *, "Valid operation: ", TRIM(line)
    END IF
  END SUBROUTINE ProcessExpression
END MODULE SymbolTable

PROGRAM SemanticAnalyzer
  USE SymbolTable
  IMPLICIT NONE
  CHARACTER(LEN=256) :: line, clean_line
  INTEGER :: ios

  OPEN(UNIT=10, FILE='INPUT.TXT', STATUS='OLD', ACTION='READ', IOSTAT=ios)
  IF (ios /= 0) THEN
    PRINT *, "Error: Unable to open file INPUT.TXT"
    STOP
  END IF
  
  PRINT *, "File opened successfully!"

  DO
    READ(10, '(A)', IOSTAT=ios) line
    IF (ios /= 0) EXIT
    PRINT *, "Read line: [", TRIM(line), "]"
    
    clean_line = RemoveComments(line)
    PRINT *, "After removing comments: [", TRIM(clean_line), "]"

    IF (LEN_TRIM(clean_line) == 0) CYCLE  

    IF (INDEX(clean_line, "=") > 0) THEN
      CALL ProcessAssignment(clean_line)
    ELSE
      CALL ProcessExpression(clean_line)
    END IF
  END DO
  
  CLOSE(10)
END PROGRAM SemanticAnalyzer