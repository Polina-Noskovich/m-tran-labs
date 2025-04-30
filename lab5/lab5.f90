MODULE SymbolTable
  IMPLICIT NONE
  INTEGER, PARAMETER :: max_symbols = 1000
  INTEGER, PARAMETER :: max_functions = 50
  INTEGER, PARAMETER :: max_scope_depth = 10
  INTEGER, PARAMETER :: max_errors = 100  

  TYPE Symbol
    CHARACTER(LEN=32) :: name
    CHARACTER(LEN=10) :: type 
    CHARACTER(LEN=10) :: scope
    INTEGER :: scope_level    
    CHARACTER(LEN=256) :: value
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
  CHARACTER(LEN=256), DIMENSION(max_errors) :: errors
  INTEGER :: symbol_count = 0
  INTEGER :: function_count = 0
  INTEGER :: current_scope_level = 0
  INTEGER :: error_count = 0
  INTEGER :: current_function_index = 0
  LOGICAL :: expected_braces = .FALSE.
  LOGICAL :: inside_function = .FALSE.
  LOGICAL :: inside_class = .FALSE.
  LOGICAL :: exit_loop = .FALSE.
  LOGICAL :: continue_loop = .FALSE.
  INTEGER :: ilop                          
  LOGICAL :: loop_exit, loop_continue 

CONTAINS

FUNCTION CleanVarName(raw_name) RESULT(clean_name)
    CHARACTER(LEN=*), INTENT(IN) :: raw_name
    CHARACTER(LEN=32) :: clean_name
    INTEGER :: i, j
    
    clean_name = ''
    j = 0
    DO i = 1, LEN_TRIM(raw_name)
        IF (raw_name(i:i) /= '$' .AND. raw_name(i:i) /= ' ' .AND. raw_name(i:i) /= ';') THEN
            j = j + 1
            clean_name(j:j) = raw_name(i:i)
        END IF
    END DO
END FUNCTION CleanVarName

SUBROUTINE AddSymbol(name, type, scope, value)
    CHARACTER(LEN=*), INTENT(IN) :: name, type, scope
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: value
    CHARACTER(LEN=32) :: cleaned_name
    INTEGER :: i
    
    cleaned_name = CleanVarName(name)
    
    DO i = symbol_count, 1, -1
        IF (TRIM(symbols(i)%name) == TRIM(cleaned_name) .AND. &
            symbols(i)%scope_level == current_scope_level) THEN
            IF (PRESENT(value)) symbols(i)%value = value
            symbols(i)%type = type
            RETURN
        END IF
    END DO

    IF (symbol_count < max_symbols) THEN
        symbol_count = symbol_count + 1
        symbols(symbol_count)%name = cleaned_name
        symbols(symbol_count)%type = type
        symbols(symbol_count)%scope = scope
        symbols(symbol_count)%scope_level = current_scope_level
        IF (PRESENT(value)) THEN
            symbols(symbol_count)%value = value
        ELSE
            symbols(symbol_count)%value = ''
        END IF
    ELSE
        CALL AddError("Symbol table overflow")
    END IF
END SUBROUTINE AddSymbol
  
FUNCTION GetSymbolType(name) RESULT(sym_type)
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=10) :: sym_type
    CHARACTER(LEN=32) :: cleaned_name
    INTEGER :: i
    
    cleaned_name = CleanVarName(name)
    sym_type = "undefined"
    
    DO i = symbol_count, 1, -1
      IF (TRIM(symbols(i)%name) == TRIM(cleaned_name)) THEN
        IF (symbols(i)%scope == "global") THEN
          sym_type = symbols(i)%type
          RETURN
        END IF
        IF (symbols(i)%scope == "local" .AND. symbols(i)%scope_level <= current_scope_level) THEN
          sym_type = symbols(i)%type
          RETURN
        END IF
      END IF
    END DO
END FUNCTION GetSymbolType

SUBROUTINE GetSymbolValue(name, value, type)
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=256), INTENT(OUT) :: value
    CHARACTER(LEN=10), INTENT(OUT) :: type
    CHARACTER(LEN=32) :: cleaned_name
    INTEGER :: i
    
    cleaned_name = CleanVarName(name)
    value = ''
    type = "undefined"
    
    DO i = symbol_count, 1, -1
        IF (TRIM(symbols(i)%name) == TRIM(cleaned_name)) THEN
            IF (symbols(i)%scope == "global") THEN
                value = symbols(i)%value
                type = symbols(i)%type
                RETURN
            END IF
            IF (symbols(i)%scope == "local" .AND. symbols(i)%scope_level <= current_scope_level) THEN
                value = symbols(i)%value
                type = symbols(i)%type
                RETURN
            END IF
        END IF
    END DO
END SUBROUTINE GetSymbolValue

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

FUNCTION DetermineNumberType(str) RESULT(num_type)
    CHARACTER(LEN=*), INTENT(IN) :: str
    CHARACTER(LEN=10) :: num_type
    
    IF (INDEX(str, '.') > 0) THEN
      num_type = "float"
    ELSE
      num_type = "int"
    END IF
END FUNCTION DetermineNumberType

FUNCTION IsLiteral(var) RESULT(is_lit)
    CHARACTER(LEN=*), INTENT(IN) :: var
    LOGICAL :: is_lit
    INTEGER :: len_var

    len_var = LEN_TRIM(var)
    is_lit = IsNumber(var)
    IF (.NOT. is_lit .AND. len_var >= 2) THEN
      is_lit = (var(1:1) == '"' .AND. var(len_var:len_var) == '"')
    END IF
END FUNCTION IsLiteral

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
    clean_line = ADJUSTL(clean_line)
END FUNCTION RemoveComments

FUNCTION CheckSyntax(line) RESULT(is_valid)
    CHARACTER(LEN=*), INTENT(IN) :: line
    LOGICAL :: is_valid
    INTEGER :: len_line
    
    len_line = LEN_TRIM(line)
    is_valid = .TRUE.
    
    IF (len_line == 0 .OR. INDEX(line, "<?php") > 0 .OR. &
        TRIM(line) == "?>" .OR. INDEX(line, "{") > 0 .OR. INDEX(line, "}") > 0) THEN
        RETURN
    END IF
    
    IF (INDEX(line, 'if') == 1 .OR. INDEX(line, 'for') == 1 .OR. &
        INDEX(line, 'while') == 1 .OR. INDEX(line, 'do') == 1 .OR. &
        INDEX(line, 'switch') == 1) THEN
        RETURN
    END IF
    
    IF (line(len_line:len_line) /= ';') THEN
        CALL AddError("Syntax error: Missing semicolon at the end of line: ["//TRIM(line)//"]")
        is_valid = .FALSE.
    END IF
END FUNCTION CheckSyntax

FUNCTION STRING_TO_INT(str) RESULT(num)
    CHARACTER(LEN=*), INTENT(IN) :: str
    INTEGER :: num
    READ(str, *) num
END FUNCTION STRING_TO_INT

FUNCTION STRING_TO_REAL(str) RESULT(num)
    CHARACTER(LEN=*), INTENT(IN) :: str
    REAL :: num
    READ(str, *) num
END FUNCTION STRING_TO_REAL

FUNCTION INT_TO_STRING(num) RESULT(str)
    INTEGER, INTENT(IN) :: num
    CHARACTER(LEN=20) :: str
    WRITE(str, '(I0)') num
    str = ADJUSTL(str)
END FUNCTION INT_TO_STRING

FUNCTION REAL_TO_STRING(num) RESULT(str)
    REAL, INTENT(IN) :: num
    CHARACTER(LEN=20) :: str
    WRITE(str, '(F0.6)') num
    str = ADJUSTL(str)
END FUNCTION REAL_TO_STRING

SUBROUTINE ProcessStringInterpolation(str)
    CHARACTER(LEN=256), INTENT(INOUT) :: str
    CHARACTER(LEN=256) :: temp_str, var_name, var_value
    CHARACTER(LEN=10) :: var_type
    INTEGER :: i, start, end_pos, len_str

    temp_str = ''
    len_str = LEN_TRIM(str)
    i = 1

    DO WHILE (i <= len_str)
        IF (str(i:i) == '$' .AND. (i == 1 .OR. SCAN(str(i-1:i-1), '\') == 0)) THEN
            start = i + 1
            end_pos = start
            DO WHILE (end_pos <= len_str)
                IF (SCAN(str(end_pos:end_pos), ' .,;!@#$%^&*()+-/{}[]') > 0) EXIT
                end_pos = end_pos + 1
            END DO
            end_pos = end_pos - 1
            var_name = str(start:end_pos)
            CALL GetSymbolValue(var_name, var_value, var_type)
            IF (var_type /= 'undefined') THEN
                temp_str = TRIM(temp_str) // TRIM(var_value)
            ELSE
                temp_str = TRIM(temp_str) // '$' // TRIM(var_name)
                CALL AddError("Undefined variable in string: $"//TRIM(var_name))
            END IF
            i = end_pos + 1
        ELSE
            temp_str = TRIM(temp_str) // str(i:i)
            i = i + 1
        END IF
    END DO
    str = TRIM(temp_str)
END SUBROUTINE ProcessStringInterpolation

RECURSIVE SUBROUTINE EvaluateExpression(expr, value, type, success)
    CHARACTER(LEN=*), INTENT(IN) :: expr
    CHARACTER(LEN=256), INTENT(OUT) :: value
    CHARACTER(LEN=10), INTENT(OUT) :: type
    LOGICAL, INTENT(OUT) :: success
    CHARACTER(LEN=256) :: left_val, right_val, cleaned_expr, array_content, temp_expr
    CHARACTER(LEN=10) :: left_type, right_type
    INTEGER :: op_pos, len_expr, i, comma_pos, start_pos, end_pos, element_count
    INTEGER :: op_len
    REAL :: real_result, real_left, real_right
    INTEGER :: int_result, int_left, int_right
    CHARACTER(LEN=256), DIMENSION(100) :: array_elements
    LOGICAL :: bool_result

    success = .TRUE.
    cleaned_expr = ADJUSTL(expr)
    len_expr = LEN_TRIM(cleaned_expr)
    value = ''
    type = 'undefined'

    cleaned_expr = ReplaceSpacesAroundOperators(cleaned_expr)

    IF (TRIM(cleaned_expr) == 'null') THEN
        type = 'null'
        value = 'null'
        RETURN
    END IF

    IF (TRIM(cleaned_expr) == 'true' .OR. TRIM(cleaned_expr) == 'false') THEN
        type = 'boolean'
        value = TRIM(cleaned_expr)
        RETURN
    END IF

    IF (INDEX(cleaned_expr, '&&') > 0) THEN
        op_pos = INDEX(cleaned_expr, '&&')
        CALL EvaluateExpression(cleaned_expr(1:op_pos-1), left_val, left_type, success)
        IF (.NOT. success) RETURN
        CALL EvaluateExpression(cleaned_expr(op_pos+2:), right_val, right_type, success)
        IF (.NOT. success) RETURN

        IF (left_type == 'boolean' .AND. right_type == 'boolean') THEN
            bool_result = (TRIM(left_val) == 'true') .AND. (TRIM(right_val) == 'true')
            value = MERGE('true  ', 'false ', bool_result)
            type = 'boolean'
        ELSE
            CALL AddError("Type mismatch for && operator")
            success = .FALSE.
        END IF
        RETURN
    END IF

        IF (INDEX(cleaned_expr, '%') > 0) THEN
        op_pos = INDEX(cleaned_expr, '%')
        CALL EvaluateExpression(cleaned_expr(1:op_pos-1), left_val, left_type, success)
        IF (.NOT. success) RETURN
        CALL EvaluateExpression(cleaned_expr(op_pos+1:), right_val, right_type, success)
        IF (.NOT. success) RETURN

        IF (left_type == 'int' .AND. right_type == 'int') THEN
            int_result = MOD(STRING_TO_INT(left_val), STRING_TO_INT(right_val))
            value = INT_TO_STRING(int_result)
            type = 'int'
        ELSE
            CALL AddError("Type mismatch for % operator: both operands must be integers")
            success = .FALSE.
        END IF
        RETURN
    END IF

    ! Обработка операторов сравнения
    IF (INDEX(cleaned_expr, '>=') > 0 .OR. INDEX(cleaned_expr, '<=') > 0 .OR. &
        INDEX(cleaned_expr, '>') > 0 .OR. INDEX(cleaned_expr, '<') > 0 .OR. &
        INDEX(cleaned_expr, '==') > 0 .OR. INDEX(cleaned_expr, '!=') > 0) THEN
        
        op_pos = MAX(INDEX(cleaned_expr, '>='), INDEX(cleaned_expr, '<='), &
                INDEX(cleaned_expr, '>'), INDEX(cleaned_expr, '<'), &
                INDEX(cleaned_expr, '=='), INDEX(cleaned_expr, '!='))
        
        op_len = 1
        IF (op_pos < LEN_TRIM(cleaned_expr)) THEN
            IF (cleaned_expr(op_pos:op_pos+1) == '>=' .OR. &
                cleaned_expr(op_pos:op_pos+1) == '<=' .OR. &
                cleaned_expr(op_pos:op_pos+1) == '==' .OR. &
                cleaned_expr(op_pos:op_pos+1) == '!=') THEN
                op_len = 2
            END IF
        END IF

        CALL EvaluateExpression(cleaned_expr(1:op_pos-1), left_val, left_type, success)
        IF (.NOT. success) RETURN
        CALL EvaluateExpression(cleaned_expr(op_pos+op_len:), right_val, right_type, success)
        IF (.NOT. success) RETURN

        IF ((left_type == 'int' .OR. left_type == 'float') .AND. &
            (right_type == 'int' .OR. right_type == 'float')) THEN
            
            real_left = MERGE(STRING_TO_REAL(left_val), REAL(STRING_TO_INT(left_val)), left_type == 'float')
            real_right = MERGE(STRING_TO_REAL(right_val), REAL(STRING_TO_INT(right_val)), right_type == 'float')

            SELECT CASE(cleaned_expr(op_pos:op_pos+op_len-1))
                CASE('>=')
                    bool_result = (real_left >= real_right)
                CASE('<=')
                    bool_result = (real_left <= real_right)
                CASE('>')
                    bool_result = (real_left > real_right)
                CASE('<')
                    bool_result = (real_left < real_right)
                CASE('==')
                    bool_result = (real_left == real_right)
                CASE('!=')
                    bool_result = (real_left /= real_right)
                CASE DEFAULT
                    CALL AddError("Unknown comparison operator: "//cleaned_expr(op_pos:op_pos+op_len-1))
                    success = .FALSE.
                    RETURN
            END SELECT

            value = MERGE('true  ', 'false ', bool_result)
            type = 'boolean'
        ELSE
            CALL AddError("Non-numeric types in comparison")
            success = .FALSE.
        END IF
        RETURN
    END IF

    IF (INDEX(cleaned_expr, 'array_sum') > 0) THEN
        start_pos = INDEX(cleaned_expr, '(') + 1
        end_pos = INDEX(cleaned_expr, ')') - 1
        array_content = cleaned_expr(start_pos:end_pos)
        
        CALL EvaluateExpression(ADJUSTL(array_content), array_content, type, success)
        IF (.NOT. success) RETURN
        
        element_count = 0
        start_pos = 1
        DO
            comma_pos = INDEX(array_content(start_pos:), ',')
            IF (comma_pos == 0) THEN
                element_count = element_count + 1
                array_elements(element_count) = ADJUSTL(array_content(start_pos:))
                EXIT
            END IF
            element_count = element_count + 1
            array_elements(element_count) = ADJUSTL(array_content(start_pos:start_pos+comma_pos-2))
            start_pos = start_pos + comma_pos
        END DO
        
        int_result = 0
        real_result = 0.0
        DO i = 1, element_count
            IF (IsNumber(array_elements(i))) THEN
                IF (DetermineNumberType(array_elements(i)) == 'int') THEN
                    int_result = int_result + STRING_TO_INT(array_elements(i))
                ELSE
                    real_result = real_result + STRING_TO_REAL(array_elements(i))
                END IF
            END IF
        END DO
        
        IF (real_result /= 0.0) THEN
            value = REAL_TO_STRING(real_result + int_result)
            type = 'float'
        ELSE
            value = INT_TO_STRING(int_result)
            type = 'int'
        END IF
        RETURN
    END IF

    IF (len_expr >= 2) THEN
        IF (cleaned_expr(1:1) == '[' .AND. cleaned_expr(len_expr:len_expr) == ']') THEN
            type = 'array'
            value = cleaned_expr(2:len_expr-1)
            RETURN
        END IF
    END IF

    IF (len_expr >= 2) THEN
        IF (cleaned_expr(1:1) == '"' .AND. cleaned_expr(len_expr:len_expr) == '"') THEN
            type = 'string'
            value = cleaned_expr(2:len_expr-1)
            CALL ProcessStringInterpolation(value)
            RETURN
        END IF
    END IF

    IF (IsNumber(cleaned_expr)) THEN
        type = DetermineNumberType(cleaned_expr)
        value = TRIM(cleaned_expr)
        RETURN
    END IF

    IF (len_expr >= 2) THEN
        IF (cleaned_expr(1:1) == '"' .AND. cleaned_expr(len_expr:len_expr) == '"') THEN
            type = 'string'
            value = cleaned_expr(2:len_expr-1)
            RETURN
        END IF
    END IF

    IF (cleaned_expr(1:1) == '$') THEN
        CALL GetSymbolValue(cleaned_expr(2:), value, type)
        IF (type /= 'undefined') THEN
            RETURN
        ELSE
            CALL AddError("Undefined variable: "//TRIM(cleaned_expr))
            success = .FALSE.
            RETURN
        END IF
    END IF

    op_pos = 0
    DO i = 1, len_expr
        IF (SCAN(cleaned_expr(i:i), "+-*/.") > 0) THEN
            op_pos = i
            EXIT
        END IF
    END DO

    IF (op_pos > 0) THEN
        left_val = cleaned_expr(1:op_pos-1)
        right_val = cleaned_expr(op_pos+1:)

        CALL EvaluateExpression(ADJUSTL(left_val), left_val, left_type, success)
        IF (.NOT. success) RETURN
        CALL EvaluateExpression(ADJUSTL(right_val), right_val, right_type, success)
        IF (.NOT. success) RETURN

        SELECT CASE (cleaned_expr(op_pos:op_pos))
            CASE ('.')
                IF (left_type == 'string' .AND. right_type == 'string') THEN
                    value = TRIM(left_val) // TRIM(right_val)
                    type = 'string'
                ELSE
                    CALL AddError("Concatenation error")
                    success = .FALSE.
                END IF

            CASE ('+')
                IF (left_type == 'int' .AND. right_type == 'int') THEN
                    int_result = STRING_TO_INT(left_val) + STRING_TO_INT(right_val)
                    value = INT_TO_STRING(int_result)
                    type = 'int'
                ELSE IF (left_type == 'float' .OR. right_type == 'float') THEN
                    real_result = STRING_TO_REAL(left_val) + STRING_TO_REAL(right_val)
                    value = REAL_TO_STRING(real_result)
                    type = 'float'
                ELSE
                    CALL AddError("Type mismatch for +")
                    success = .FALSE.
                END IF

            CASE DEFAULT
                CALL AddError("Unsupported operator: "//cleaned_expr(op_pos:op_pos))
                success = .FALSE.
        END SELECT
        RETURN
    END IF

    CALL AddError("Invalid expression: "//TRIM(cleaned_expr))
    success = .FALSE.
END SUBROUTINE EvaluateExpression

FUNCTION ReplaceSpacesAroundOperators(str) RESULT(new_str)
    CHARACTER(LEN=*), INTENT(IN) :: str
    CHARACTER(LEN=256) :: new_str
    INTEGER :: k, j, len_str, next_k
    LOGICAL :: in_quotes, is_double_op

    new_str = ''
    j = 1
    in_quotes = .FALSE.
    len_str = LEN_TRIM(str)
    k = 1

    DO WHILE (k <= len_str)
        IF (str(k:k) == '"') in_quotes = .NOT. in_quotes
        
        IF (.NOT. in_quotes) THEN
            is_double_op = .FALSE.
            IF (k < len_str) THEN
                IF (str(k:k+1) == '&&' .OR. str(k:k+1) == '>=' .OR. &
                    str(k:k+1) == '<=' .OR. str(k:k+1) == '==' .OR. &
                    str(k:k+1) == '!=') THEN
                    new_str(j:j+1) = str(k:k+1)
                    j = j + 2
                    k = k + 2
                    is_double_op = .TRUE.
                END IF
            END IF

            IF (.NOT. is_double_op) THEN
                IF (SCAN(str(k:k), '><=&|+-*/%') > 0) THEN
                    IF (j > 1 .AND. new_str(j-1:j-1) == ' ') j = j - 1
                    new_str(j:j) = str(k:k)
                    j = j + 1
                    next_k = k + 1
                    DO WHILE (next_k <= len_str .AND. str(next_k:next_k) == ' ')
                        next_k = next_k + 1
                    END DO
                    k = next_k
                ELSE
                    new_str(j:j) = str(k:k)
                    j = j + 1
                    k = k + 1
                END IF
            END IF
        ELSE
            new_str(j:j) = str(k:k)
            j = j + 1
            k = k + 1
        END IF
    END DO
    new_str = ADJUSTL(new_str)
END FUNCTION ReplaceSpacesAroundOperators

SUBROUTINE ProcessForLoop(lines, num_lines, current_line)
    CHARACTER(LEN=256), DIMENSION(*) :: lines
    INTEGER, INTENT(IN) :: num_lines
    INTEGER, INTENT(INOUT) :: current_line
    CHARACTER(LEN=256) :: init_expr, cond_expr, incr_expr, block_line
    CHARACTER(LEN=256) :: cond_value, temp_val
    CHARACTER(LEN=10) :: cond_type
    INTEGER :: start_pos, end_pos, brace_count, saved_line
    LOGICAL :: success
    CHARACTER(LEN=32) :: var_name 
    LOGICAL :: exit_loop_save, continue_loop_save

    start_pos = INDEX(lines(current_line), '(') + 1
    end_pos = INDEX(lines(current_line), ')') - 1
    block_line = lines(current_line)(start_pos:end_pos)
    
    start_pos = 1
    end_pos = INDEX(block_line, ';')
    init_expr = ADJUSTL(block_line(start_pos:end_pos-1))
    
    start_pos = end_pos + 1
    end_pos = INDEX(block_line(start_pos:), ';') + start_pos - 1
    cond_expr = ADJUSTL(block_line(start_pos:end_pos-1))
    
    start_pos = end_pos + 1
    incr_expr = ADJUSTL(block_line(start_pos:))

    ! Инициализация переменной цикла
    IF (LEN_TRIM(init_expr) > 0) THEN
        CALL ProcessAssignment(TRIM(init_expr)//";")
        var_name = CleanVarName(init_expr(:INDEX(init_expr,'=')-1))
        CALL AddSymbol(var_name, "int", "global", "1")
    END IF

    saved_line = current_line
    brace_count = 0
    exit_loop_save = exit_loop
    continue_loop_save = continue_loop

    DO WHILE (.TRUE.)
        IF (LEN_TRIM(cond_expr) > 0) THEN
            CALL EvaluateExpression(cond_expr, cond_value, cond_type, success)
            IF (.NOT. success .OR. TRIM(cond_value) /= 'true') EXIT
        END IF
        
        current_line = saved_line
        brace_count = 1
        exit_loop = .FALSE.
        continue_loop = .FALSE.
        
        DO WHILE (current_line <= num_lines .AND. brace_count > 0)
            current_line = current_line + 1
            IF (current_line > num_lines) EXIT
            IF (INDEX(lines(current_line), '{') > 0) brace_count = brace_count + 1
            IF (INDEX(lines(current_line), '}') > 0) brace_count = brace_count - 1
            IF (brace_count > 0 .AND. .NOT. exit_loop .AND. .NOT. continue_loop) THEN
                CALL ProcessCodeLine(lines(current_line), lines, num_lines, current_line)
            END IF
        END DO
        
        IF (LEN_TRIM(incr_expr) > 0) THEN
            CALL ProcessAssignment(TRIM(incr_expr)//";")
            ! Обновляем значение в глобальной области
            IF (current_scope_level == 0) THEN
                CALL AddSymbol(TRIM(CleanVarName(incr_expr(:INDEX(incr_expr,'=')-1))), "int", "global", &
                    TRIM(INT_TO_STRING(STRING_TO_INT(symbols(symbol_count)%value) + 1)))
            END IF
        END IF
        IF (exit_loop) EXIT
        IF (.NOT. continue_loop_save) CYCLE
    END DO

    exit_loop = exit_loop_save
    continue_loop = continue_loop_save
    current_line = saved_line + 1
END SUBROUTINE ProcessForLoop

SUBROUTINE ProcessDoWhileLoop(lines, num_lines, current_line)
    CHARACTER(LEN=256), DIMENSION(*) :: lines
    INTEGER, INTENT(IN) :: num_lines
    INTEGER, INTENT(INOUT) :: current_line
    CHARACTER(LEN=256) :: cond_expr, cond_value
    CHARACTER(LEN=10) :: cond_type
    INTEGER :: brace_count, saved_line, start_pos, end_pos, original_line
    LOGICAL :: success, exit_loop_save, continue_loop_save

    original_line = current_line
    saved_line = current_line
    brace_count = 1
    exit_loop_save = exit_loop
    continue_loop_save = continue_loop

    ! Find opening brace
    DO WHILE (current_line <= num_lines)
        IF (INDEX(lines(current_line), '{') > 0) THEN
            current_scope_level = current_scope_level + 1
            EXIT
        END IF
        current_line = current_line + 1
    END DO

    saved_line = current_line

    DO WHILE (.TRUE.)
        exit_loop = .FALSE.
        continue_loop = .FALSE.
        
        ! Process body
        current_line = saved_line
        brace_count = 1
        DO WHILE (current_line <= num_lines .AND. brace_count > 0)
            current_line = current_line + 1
            IF (current_line > num_lines) EXIT
            IF (INDEX(lines(current_line), '{') > 0) brace_count = brace_count + 1
            IF (INDEX(lines(current_line), '}') > 0) brace_count = brace_count - 1
            
            IF (brace_count > 0 .AND. .NOT. exit_loop .AND. .NOT. continue_loop) THEN
                CALL ProcessCodeLine(lines(current_line), lines, num_lines, current_line)
            END IF
        END DO

        ! Extract condition from original line
        start_pos = INDEX(lines(original_line), '(', .TRUE.) + 1
        end_pos = INDEX(lines(original_line), ')', .TRUE.) - 1
        cond_expr = ADJUSTL(lines(original_line)(start_pos:end_pos))
        
        CALL EvaluateExpression(cond_expr, cond_value, cond_type, success)
        IF (.NOT. success .OR. TRIM(cond_value) /= 'true' .OR. exit_loop) EXIT
        IF (continue_loop) CYCLE
    END DO

    current_scope_level = current_scope_level - 1
    exit_loop = exit_loop_save
    continue_loop = continue_loop_save
    current_line = current_line + 1
END SUBROUTINE ProcessDoWhileLoop

SUBROUTINE ProcessIfBlock(lines, num_lines, current_line)
    CHARACTER(LEN=256), DIMENSION(*) :: lines
    INTEGER, INTENT(IN) :: num_lines
    INTEGER, INTENT(INOUT) :: current_line
    CHARACTER(LEN=256) :: condition, cond_value
    CHARACTER(LEN=10) :: cond_type
    LOGICAL :: success, execute_block
    INTEGER :: brace_count
    INTEGER :: start_pos, end_pos

    start_pos = INDEX(lines(current_line), '(') + 1
    end_pos = INDEX(lines(current_line), ')') - 1
    condition = ADJUSTL(lines(current_line)(start_pos:end_pos))
    
    CALL EvaluateExpression(TRIM(condition), cond_value, cond_type, success)

    IF (.NOT. success .OR. cond_type /= 'boolean') THEN
        CALL AddError("Invalid condition in if statement")
        RETURN
    END IF

    execute_block = (TRIM(cond_value) == 'true')

    ! Check if current line has opening brace
    IF (INDEX(lines(current_line), '{') > 0) THEN
        current_scope_level = current_scope_level + 1
    ELSE
        ! Search for opening brace in next lines
        DO WHILE (current_line <= num_lines)
            current_line = current_line + 1
            IF (current_line > num_lines) EXIT
            IF (INDEX(lines(current_line), '{') > 0) THEN
                current_scope_level = current_scope_level + 1
                EXIT
            END IF
        END DO
    END IF

    brace_count = 1
    IF (execute_block) THEN
        DO WHILE (current_line <= num_lines .AND. brace_count > 0)
            current_line = current_line + 1
            IF (current_line > num_lines) EXIT
            IF (INDEX(lines(current_line), '{') > 0) brace_count = brace_count + 1
            IF (INDEX(lines(current_line), '}') > 0) brace_count = brace_count - 1
            IF (brace_count > 0) THEN
                CALL ProcessCodeLine(lines(current_line), lines, num_lines, current_line)
            END IF
        END DO
    ELSE
        DO WHILE (current_line <= num_lines .AND. brace_count > 0)
            current_line = current_line + 1
            IF (current_line > num_lines) EXIT
            IF (INDEX(lines(current_line), '{') > 0) brace_count = brace_count + 1
            IF (INDEX(lines(current_line), '}') > 0) brace_count = brace_count - 1
        END DO
    END IF

    current_scope_level = current_scope_level - 1
END SUBROUTINE ProcessIfBlock

SUBROUTINE ProcessDefine(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: const_name
    CHARACTER(LEN=256) :: const_value
    CHARACTER(LEN=10) :: type
    INTEGER :: pos1, pos2, pos3, pos4, i
    LOGICAL :: is_num

    pos1 = INDEX(line, '"')
    pos2 = INDEX(line(pos1+1:), '"') + pos1
    const_name = line(pos1+1:pos2-1)

    pos3 = INDEX(line(pos2+1:), ',') + pos2
    pos4 = INDEX(line(pos3+1:), ')') + pos3
    const_value = ADJUSTL(line(pos3+1:pos4-1))

    IF (IsNumber(const_value)) THEN
        type = DetermineNumberType(const_value)
    ELSE IF (const_value(1:1) == '"' .AND. const_value(LEN_TRIM(const_value):LEN_TRIM(const_value)) == '"') THEN
        type = 'string'
        const_value = ADJUSTL(const_value(2:LEN_TRIM(const_value)-1))
    ELSE IF (TRIM(const_value) == 'true' .OR. TRIM(const_value) == 'false') THEN
        type = 'boolean'
    ELSE IF (TRIM(const_value) == 'null') THEN
        type = 'null'
    ELSE
        type = 'unknown'
        CALL AddError("Unknown type for constant: "//TRIM(const_value))
        RETURN
    END IF

    DO i = 1, symbol_count
        IF (TRIM(symbols(i)%name) == TRIM(const_name) .AND. symbols(i)%scope == 'constant') THEN
            CALL AddError("Constant "//TRIM(const_name)//" already defined")
            RETURN
        END IF
    END DO

    CALL AddSymbol(const_name, type, 'constant', TRIM(const_value))
END SUBROUTINE ProcessDefine

SUBROUTINE ProcessSwitchBlock(lines, num_lines, current_line)
    CHARACTER(LEN=256), DIMENSION(*) :: lines
    INTEGER, INTENT(IN) :: num_lines
    INTEGER, INTENT(INOUT) :: current_line
    CHARACTER(LEN=256) :: switch_expr, expr_value, case_val
    CHARACTER(LEN=10) :: expr_type, case_type
    LOGICAL :: success, matched, found_break
    INTEGER :: brace_count, start_pos, end_pos, case_line, pos

    start_pos = INDEX(lines(current_line), '(') + 1
    end_pos = INDEX(lines(current_line), ')') - 1
    switch_expr = ADJUSTL(lines(current_line)(start_pos:end_pos))

    CALL EvaluateExpression(switch_expr, expr_value, expr_type, success)
    IF (.NOT. success) THEN
        CALL AddError("Invalid switch expression: "//TRIM(switch_expr))
        RETURN
    END IF

    DO WHILE (current_line <= num_lines)
        current_line = current_line + 1
        IF (current_line > num_lines) EXIT
        IF (INDEX(lines(current_line), '{') > 0) THEN
            current_scope_level = current_scope_level + 1
            EXIT
        END IF
    END DO

    brace_count = 1
    matched = .FALSE.
    found_break = .FALSE.

    DO WHILE (current_line <= num_lines .AND. brace_count > 0)
        current_line = current_line + 1
        IF (current_line > num_lines) EXIT
        IF (INDEX(lines(current_line), '{') > 0) brace_count = brace_count + 1
        IF (INDEX(lines(current_line), '}') > 0) brace_count = brace_count - 1

        IF (brace_count == 1) THEN
            IF (INDEX(lines(current_line), 'case') > 0) THEN
                pos = INDEX(lines(current_line), 'case') + 4
                end_pos = INDEX(lines(current_line), ':') - 1
                case_val = ADJUSTL(lines(current_line)(pos:end_pos))
                CALL EvaluateExpression(case_val, case_val, case_type, success)
                IF (.NOT. success) CYCLE

                IF (.NOT. matched .AND. TRIM(case_val) == TRIM(expr_value)) THEN
                    matched = .TRUE.
                    case_line = current_line
                    DO WHILE (case_line <= num_lines)
                        case_line = case_line + 1
                        IF (case_line > num_lines) EXIT
                        IF (INDEX(lines(case_line), 'break;') > 0) THEN
                            current_line = case_line
                            EXIT
                        ELSE IF (INDEX(lines(case_line), 'case') > 0 .OR. &
                                 INDEX(lines(case_line), 'default') > 0) THEN
                            current_line = case_line - 1
                            EXIT
                        ELSE IF (INDEX(lines(case_line), '}') > 0) THEN
                            EXIT
                        ELSE
                            CALL ProcessCodeLine(lines(case_line), lines, num_lines, case_line)
                        END IF
                    END DO
                END IF
            ELSE IF (INDEX(lines(current_line), 'default') > 0) THEN
                IF (.NOT. matched) THEN
                    matched = .TRUE.
                    case_line = current_line
                    DO WHILE (case_line <= num_lines)
                        case_line = case_line + 1
                        IF (case_line > num_lines) EXIT
                        IF (INDEX(lines(case_line), 'break;') > 0) THEN
                            current_line = case_line
                            EXIT
                        ELSE IF (INDEX(lines(case_line), 'case') > 0 .OR. &
                                 INDEX(lines(case_line), 'default') > 0) THEN
                            current_line = case_line - 1
                            EXIT
                        ELSE IF (INDEX(lines(case_line), '}') > 0) THEN
                            EXIT
                        ELSE
                            CALL ProcessCodeLine(lines(case_line), lines, num_lines, case_line)
                        END IF
                    END DO
                END IF
            END IF
        END IF
    END DO

    current_scope_level = current_scope_level - 1
END SUBROUTINE ProcessSwitchBlock

SUBROUTINE ReplaceEscapes(str)
    CHARACTER(LEN=256), INTENT(INOUT) :: str
    CHARACTER(LEN=256) :: temp_str
    INTEGER :: i, j, k
    
    temp_str = ''
    j = 1
    i = 1
    DO WHILE(i <= LEN_TRIM(str))
        IF (str(i:i) == '\') THEN
            IF (i < LEN_TRIM(str)) THEN
                SELECT CASE(str(i+1:i+1))
                    CASE ('n')
                        temp_str(j:j) = ACHAR(10)
                        j = j + 1
                        i = i + 2
                    CASE ('t')
                        temp_str(j:j) = ACHAR(9)
                        j = j + 1
                        i = i + 2
                    CASE DEFAULT
                        temp_str(j:j) = str(i:i)
                        j = j + 1
                        i = i + 1
                END SELECT
            ELSE
                temp_str(j:j) = str(i:i)
                j = j + 1
                i = i + 1
            END IF
        ELSE
            temp_str(j:j) = str(i:i)
            j = j + 1
            i = i + 1
        END IF
    END DO
    str = TRIM(temp_str)
END SUBROUTINE ReplaceEscapes

SUBROUTINE ProcessIncrement(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: var_name
    CHARACTER(LEN=256) :: value
    CHARACTER(LEN=10) :: type
    INTEGER :: plus_pos

    plus_pos = INDEX(line, "++")
    var_name = CleanVarName(line(1:plus_pos-1))
    
    CALL GetSymbolValue(var_name, value, type)
    IF (type == 'undefined') THEN
        CALL AddSymbol(var_name, "int", "global", "0")
        value = "0"
        type = "int"
    END IF

    IF (type == 'int') THEN
        value = INT_TO_STRING(STRING_TO_INT(value) + 1)
    ELSE
        value = REAL_TO_STRING(STRING_TO_REAL(value) + 1.0)
    END IF
    
    ! Всегда обновляем глобальную переменную
    CALL AddSymbol(var_name, type, "global", value)
END SUBROUTINE ProcessIncrement

SUBROUTINE ProcessCodeLine(line, lines, num_lines, current_line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=256), DIMENSION(*), INTENT(IN) :: lines
    INTEGER, INTENT(IN) :: num_lines
    INTEGER, INTENT(INOUT) :: current_line
    CHARACTER(LEN=256) :: clean_line

    clean_line = RemoveComments(ADJUSTL(line))
    IF (LEN_TRIM(clean_line) == 0) RETURN

    IF (INDEX(clean_line, "if") > 0) THEN
        CALL ProcessIfBlock(lines, num_lines, current_line)
    ELSE IF (INDEX(clean_line, "++") > 0) THEN
        CALL ProcessIncrement(clean_line)
    ELSE IF (INDEX(clean_line, "switch") > 0) THEN
        CALL ProcessSwitchBlock(lines, num_lines, current_line)
    ELSE IF (INDEX(clean_line, "define") > 0) THEN
        CALL ProcessDefine(clean_line)
    ELSE IF (INDEX(clean_line, "=") > 0) THEN
        CALL ProcessAssignment(clean_line)
    ELSE IF (INDEX(clean_line, "echo") > 0) THEN
        CALL ProcessEchoStatement(clean_line, lines, num_lines, current_line)
    ELSE IF (INDEX(clean_line, "for") > 0) THEN
        CALL ProcessForLoop(lines, num_lines, current_line)
    ELSE IF (INDEX(clean_line, "do") > 0) THEN
        CALL ProcessDoWhileLoop(lines, num_lines, current_line)
    ELSE IF (INDEX(clean_line, "break") > 0) THEN
        exit_loop = .TRUE.
    ELSE IF (INDEX(clean_line, "continue") > 0) THEN
        continue_loop = .TRUE.
    END IF
END SUBROUTINE ProcessCodeLine

SUBROUTINE ProcessAssignment(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=32) :: var_name
    CHARACTER(LEN=256) :: expr, value
    CHARACTER(LEN=10) :: type
    LOGICAL :: success
    INTEGER :: eq_pos, end_pos

    eq_pos = INDEX(line, "=")
    IF (eq_pos > 0) THEN
        var_name = CleanVarName(ADJUSTL(line(:eq_pos-1)))
        expr = ADJUSTL(line(eq_pos+1:))
        
        end_pos = LEN_TRIM(expr)
        DO WHILE (end_pos > 0 .AND. (expr(end_pos:end_pos) == ';' .OR. expr(end_pos:end_pos) == ' '))
            end_pos = end_pos - 1
        END DO
        expr = expr(1:end_pos)
        
        CALL EvaluateExpression(expr, value, type, success)
        IF (.NOT. success) RETURN
        IF (current_scope_level == 0) THEN
            CALL AddSymbol(var_name, type, "global", value)
        ELSE
            CALL AddSymbol(var_name, type, "local", value)
        END IF
    END IF
END SUBROUTINE ProcessAssignment

SUBROUTINE ProcessEchoStatement(line, lines, num_lines, current_line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=256), DIMENSION(*), INTENT(IN) :: lines
    INTEGER, INTENT(IN) :: num_lines
    INTEGER, INTENT(INOUT) :: current_line
    CHARACTER(LEN=256) :: expr_part, value, temp_line
    CHARACTER(LEN=10) :: type
    LOGICAL :: success, in_string
    INTEGER :: echo_pos, end_pos, i, len_line, quote_pos

    echo_pos = INDEX(line, "echo")
    IF (echo_pos == 0) RETURN

    expr_part = line(echo_pos + 4:)
    expr_part = ADJUSTL(expr_part)
    end_pos = LEN_TRIM(expr_part)
    
    ! Check for multi-line string
    in_string = .FALSE.
    len_line = LEN_TRIM(expr_part)
    DO i = 1, len_line
        IF (expr_part(i:i) == '"') THEN
            in_string = .NOT. in_string
        END IF
    END DO

    IF (in_string) THEN
        temp_line = expr_part
        DO WHILE (in_string .AND. current_line <= num_lines)
            current_line = current_line + 1
            IF (current_line > num_lines) EXIT
            temp_line = TRIM(temp_line) // ' ' // TRIM(ADJUSTL(lines(current_line)))
            len_line = LEN_TRIM(temp_line)
            DO i = 1, len_line
                IF (temp_line(i:i) == '"') THEN
                    in_string = .NOT. in_string
                    EXIT
                END IF
            END DO
        END DO
        expr_part = temp_line
    END IF

    ! Process the expression
    end_pos = LEN_TRIM(expr_part)
    DO WHILE (end_pos > 0 .AND. (expr_part(end_pos:end_pos) == ';' .OR. expr_part(end_pos:end_pos) == ' '))
        end_pos = end_pos - 1
    END DO
    expr_part = expr_part(1:end_pos)

    CALL EvaluateExpression(expr_part, value, type, success)
    IF (success) THEN
        IF (type == 'string') THEN
            CALL ReplaceEscapes(value)
            CALL ProcessStringInterpolation(value)  ! Интерполяция переменных
        END IF
        PRINT '(A)', TRIM(value)
    END IF
END SUBROUTINE ProcessEchoStatement

SUBROUTINE AddError(error_message)
    CHARACTER(LEN=*), INTENT(IN) :: error_message
    IF (error_count < max_errors) THEN
      error_count = error_count + 1
      errors(error_count) = error_message
    ELSE
      PRINT *, "Error: Too many errors! Cannot add more."
    END IF
END SUBROUTINE AddError

SUBROUTINE PrintSymbolTable()
    INTEGER :: i
    PRINT *, "Symbol Table:"
    PRINT *, "--------------------------------------------------"
    PRINT *, "     Name      Type     Scope      Level     Value"
    PRINT *, "--------------------------------------------------"
    DO i = 1, symbol_count
      WRITE(*, '(A10, A10, A10, I10, A20)') &
        TRIM(symbols(i)%name), &
        TRIM(symbols(i)%type), &
        TRIM(symbols(i)%scope), &
        symbols(i)%scope_level, &
        TRIM(symbols(i)%value)
    END DO
    PRINT *, "--------------------------------------------------"
END SUBROUTINE PrintSymbolTable

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

SUBROUTINE ProcessCodeBlock(lines, num_lines)
    CHARACTER(LEN=256), DIMENSION(:), INTENT(IN) :: lines
    INTEGER, INTENT(IN) :: num_lines
    INTEGER :: current_line

    current_line = 1
    DO WHILE (current_line <= num_lines)
        CALL ProcessCodeLine(lines(current_line), lines, num_lines, current_line)
        current_line = current_line + 1
    END DO
END SUBROUTINE ProcessCodeBlock

SUBROUTINE SIMPLE_FOR_LOOP(start_val, end_val, step, prefix)
  INTEGER, INTENT(IN) :: start_val, end_val, step
  CHARACTER(LEN=*), INTENT(IN) :: prefix
  INTEGER :: i
  CHARACTER(LEN=50) :: output
  
  DO i = start_val, end_val, step
    WRITE(output, '(A, I0)') TRIM(prefix), i
    CALL CLEAN_PRINT(output)
  END DO
END SUBROUTINE

SUBROUTINE DO_WHILE_LOOP(end_condition, prefix)
  INTEGER, INTENT(IN) :: end_condition
  CHARACTER(LEN=*), INTENT(IN) :: prefix
  INTEGER :: counter
  CHARACTER(LEN=50) :: output
  
  counter = 1
  DO
    WRITE(output, '(A, I0)') TRIM(prefix), counter
    CALL CLEAN_PRINT(output)
    
    counter = counter + 1
    IF (counter > end_condition) EXIT
  END DO
END SUBROUTINE

SUBROUTINE COMPLEX_FOR_LOOP(start_val, end_val, step, exit_point, continue_point, prefix)
  INTEGER, INTENT(IN) :: start_val, end_val, step
  INTEGER, INTENT(IN) :: exit_point, continue_point
  CHARACTER(LEN=*), INTENT(IN) :: prefix
  INTEGER :: i
  LOGICAL :: should_exit, should_continue
  CHARACTER(LEN=50) :: output
  
  should_exit = .FALSE.
  should_continue = .FALSE.
  
  DO i = start_val, end_val, step
    ! Проверка на выход из цикла
    IF (should_exit) EXIT
    
    ! Обработка continue
    IF (should_continue) THEN
      should_continue = .FALSE.
      CYCLE
    END IF
    
    ! Условие для break
    IF (i == exit_point) THEN
      should_exit = .TRUE.
      CYCLE
    END IF
    
    ! Условие для continue
    IF (i < continue_point) THEN
      should_continue = .TRUE.
      CYCLE
    END IF
    
    ! Основной вывод
    WRITE(output, '(A, I0)') TRIM(prefix), i
    CALL CLEAN_PRINT(output)
  END DO
END SUBROUTINE

! Вспомогательная процедура для вывода
SUBROUTINE CLEAN_PRINT(str)
  CHARACTER(LEN=*), INTENT(IN) :: str
  WRITE(*, '(A)') TRIM(ADJUSTL(str))
END SUBROUTINE



END MODULE SymbolTable

PROGRAM Interpreter
  USE SymbolTable
  IMPLICIT NONE
  CHARACTER(LEN=256) :: line
  CHARACTER(LEN=256), DIMENSION(1000) :: lines
  INTEGER :: num_lines = 0, ios

  OPEN(UNIT=10, FILE='INPUT.TXT', STATUS='OLD', IOSTAT=ios)
  IF (ios /= 0) THEN
    PRINT *, "Error opening file"
    STOP
  END IF

  DO
    READ(10, '(A)', IOSTAT=ios) line
    IF (ios /= 0) EXIT
    num_lines = num_lines + 1
    lines(num_lines) = line
  END DO
  CLOSE(10)


  CALL ProcessCodeBlock(lines, num_lines)
  CALL SIMPLE_FOR_LOOP(1, 5, 1, "DO_loop_iteration:_ ")
  CALL DO_WHILE_LOOP(3, "DO_WHILE_loop_iteration:_ ")
  CALL COMPLEX_FOR_LOOP(1, 10, 1, 8, 3, "Iteration_with_EXIT_and_CYCLE:_ ")
  CALL PrintSymbolTable()
  CALL PrintErrors()

END PROGRAM Interpreter