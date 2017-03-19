.286
; ASCII:
TAB             equ 9
SPACE           equ 32
CR              equ 13
LF              equ 10

; Consts:
LINE_PRINT      equ 9h
CHAR_PRINT      equ 2h
ARGS_LENGTH     equ 80h
ARGS_START      equ 82h

; Program controls:
EXIT            equ 4ch
DOS             equ 21h

; Params:

; In this project:
; 1 byte for version
; 32 bytes for ASCII hex
MAX_ARGS_SIZE   equ 33
MAX_ARGS_COUNT  equ 2

data segment
    ; The chessboard array stores numbers of visits in each field until
    ; `convert_visits_to_ascii` is called. After that it will contain ASCII
    ; symbols from `symbols` array.
    chessboard              db 153 dup (0)
    end_position            db 0
    border_top              db '+--[ RSA 1024]----+$'
    border_bottom           db '+-----------------+$'
    border_side             db '|'
    symbols                 db ' .o+=*BOX@%&#/^'

    err_general_info        db 'Provide version number (0 or 1) and 32 hexadecimal digits [0-9a-f].$'
    err_wrong_version       db 'Wrong version number.',CR,LF,'$'
    err_incorrect_hex       db 'Incorrect SSH fingerprint.',CR,LF,'$'

    ; All non-whitespace characters from args that are stored in memory.
    args                    db MAX_ARGS_SIZE dup (0)
    ; Array containing lengths of args.
    args_lengths            db MAX_ARGS_COUNT dup (0)
    ; Total count of used arguments.
    args_count              db 0

    ; Reusable error messages:
    err_too_long            db 'Total length of provided arguments was too long (buffer overflow).',CR,LF,'$'
    err_wrong_args_lengths  db 'Incorrect lengths of arguments.',CR,LF,'$'
    err_too_many_args       db 'Too many arguments provided.',CR,LF,'$'
    err_not_enough_args     db 'Not enough arguments provided.',CR,LF,'$'
data ends

code segment

;------------------------------------------------------------------------------
; MODULO
; Calculates AX % CL. Result of division goes to AL and of modulo to AH.
;------------------------------------------------------------------------------
modulo proc
    push    bx
    push    cx
    mov     bl, cl
    div     bl
    pop     cx
    pop     bx
    ret
modulo endp

;------------------------------------------------------------------------------
; PRINT_CHAR
; Prints character stored in AL.
;------------------------------------------------------------------------------
print_char proc
    push    ax ; Used for storing int 21h function.
    push    dx ; Used to store character to print.

    mov     ah, CHAR_PRINT
    mov     dl, al
    int     DOS

    pop     dx
    pop     ax
    ret
print_char endp

;------------------------------------------------------------------------------
; PRINT_STR
; Prints string from offset given in SI.
;------------------------------------------------------------------------------
print_str proc
    push    ax ; Used for storing int 21h function.
    push    dx ; Used for storing pointer to string.

    xor     ax, ax
    mov     dx, si
    mov     ah, LINE_PRINT
    int     DOS

    pop     dx
    pop     ax
    ret
print_str endp

;------------------------------------------------------------------------------
; NEW_LINE
; Prints new line (CR+LF) in console.
;------------------------------------------------------------------------------
new_line proc
    push    ax ; Used for storing int 21h function.
    push    dx ; Used to store character to print.

    mov     ah, CHAR_PRINT
    mov     dl, CR
    int     DOS
    mov     dl, LF
    int     DOS

    pop     dx
    pop     ax
    ret
new_line endp

;------------------------------------------------------------------------------
; EXIT_ERR source
; Prints error message and general arguments requirements and closes.
; Expects offset address in SI.
;------------------------------------------------------------------------------
exit_err proc
    call    print_str
    mov     si, offset err_general_info
    call    print_str
    call    close
exit_err endp

;------------------------------------------------------------------------------
; FLUSH_ARGS
; Increases count of args, puts length of last argument in array and resets CX
; which stores count of characters in currently processed argument.
;------------------------------------------------------------------------------
flush_arg proc
    push    ax
    push    bx

    ; Set length of last arg.
    mov     bl, ds:[args_count]
    mov     ds:[args_lengths+bx], cl

    ; Increase argument count.
    mov     al, ds:[args_count]
    inc     al
    mov     ds:[args_count], al

    ; Set counter of characters in arg to 0.
    xor     cx, cx
    pop     bx
    pop     ax
    ret
flush_arg endp

;------------------------------------------------------------------------------
; READ_ARGS
; Reads args from console to memory.
;
; Checks if arguments can be stored in memory (which means that they don't
; exceed maximum amount of args or their total length - look: params at the
; top of this file).
;
; This procedure DOESN't do any checks on correctness of arguments,
; distribution of characters between them, minimum number of arguments etc.
; Those checks should be placed in PARSE_ARGS procedure.
;------------------------------------------------------------------------------
read_args proc
    push    ax ; General calculations.
    push    bx ; Total amount of arg characters stored in memory.
    push    cx ; Counter of characters in currently processed argument.
    push    si ; Source Index - command line.
    push    di ; Destination Index - `args` array in memory.

    dec     bx  ; Offset BX because counter will be is one step ahead.
    mov     di, ARGS_START
    xor     cx, cx
    mov     si, offset args

    read_chars:
    ; Check if amount of non-whitespace characters exceeds buffer.
    ; If yes, exit with error.
    cmp     bx, MAX_ARGS_SIZE
    jg      catch_err_too_long

    ; Get next char.
    mov     al, es:[di]

    ; If it is ASCII 0 then it means no command line input was provided.
    cmp     al, 0
    je      end_of_args

    ; If it is CR user provided at least one argument but there are no more.
    cmp     al, CR
    je      check_last_arg

    cmp     al, TAB
    je      found_whitespace
    cmp     al, SPACE
    je      found_whitespace

    ; If user already provided maximum number of arguments...
    push    ax
    mov     al, MAX_ARGS_COUNT
    cmp     al, ds:[args_count]
    pop     ax
    je      catch_err_too_many_args

    ; Else, proceed storing in memory.
    mov     dl, al
    mov     [si], dl
    inc     si
    inc     cx
    inc     bx
    jmp     inc_iterator

    found_whitespace:
    ; If CX is not equal to 0 it means an argument just ended.
    cmp     cx, 0
    je      inc_iterator

    ; Update info about arguments in memory.
    call    flush_arg

    inc_iterator:
    inc     di
    jmp     read_chars

    ; If user provided at least one arg CX won't be equal to 0 and last arg
    ; should be past like others. If not, jump straight to exit.
    check_last_arg:
    cmp     cx, 0
    je      end_of_args

    call    flush_arg

    end_of_args:
    pop     di
    pop     si
    pop     cx
    pop     bx
    pop     ax
    ret

    catch_err_too_long:
    mov     si, offset err_too_long
    call    exit_err 

    catch_err_too_many_args:
    mov     si, offset err_too_many_args
    call    exit_err
read_args endp

;------------------------------------------------------------------------------
; PARSE_ARGS
; Check if first argument is '0' or '1' and second has length equal 16 and
; contains only hexadecimal digits [0-9a-f].
;------------------------------------------------------------------------------
parse_args proc
    push    ax ; General calculations.
    push    cx ; Loop counter.
    push    di ; Pointer in array.

    ; Check if user provided *exactly* two arguments.
    xor     ax, ax
    mov     al, ds:[args_count]
    cmp     ax, 2
    jl      catch_err_not_enough_args

    ; Check if length of the first provided argument is 1 and length of the
    ; second one is 32.
    mov     al, ds:[args_lengths]
    cmp     ax, 1
    jne     catch_error_wrong_args_lengths

    mov     al, ds:[args_lengths+1]
    cmp     ax, 32
    jne     catch_error_wrong_args_lengths

    ; Check if first argument is either 0 or 1.
    check_first_arg:

    ; Convert ASCII digit to binary.
    mov     al, ds:[args]
    sub     al, '0'
    mov     ds:[args], al

    ; Check number.
    cmp     ax, 0
    je      check_second_arg
    cmp     ax, 1
    je      check_second_arg
    jmp     catch_err_wrong_version

    ; Check if second argument fullfils regex [0-9a-f].
    check_second_arg:
    mov     cx, 32  ; We are looking for 32 characters.
    mov     di, 1   ; Starting with 1 because [0] is version number.

    check_chars:
    mov     al, ds:[args+di]

    ; if (48 <= x <= 57 or 97 <= x <= 102)
    cmp     ax, '0'
    jl      catch_err_incorrect_hex
    cmp     ax, 'f'
    jg      catch_err_incorrect_hex
    cmp     ax, '9'
    jle     ok
    cmp     ax, 'a'
    jge     ok
    jmp     catch_err_incorrect_hex

    ok:
    inc     di
    loop    check_chars

    pop     di
    pop     cx
    pop     ax
    ret

    catch_err_not_enough_args:
    mov     si, offset err_not_enough_args
    call    exit_err

    catch_error_wrong_args_lengths:
    mov     si, offset err_wrong_args_lengths
    call    exit_err

    catch_err_wrong_version:
    mov     si, offset err_wrong_version
    call    exit_err

    catch_err_incorrect_hex:
    mov     si, offset err_incorrect_hex
    call    exit_err
parse_args endp

;------------------------------------------------------------------------------
; HEX_PAIR_TO_BYTE
; Expects two hex digits, first in AH and the second one in AL.
; Returns result in AL, resets AH to 0.
;------------------------------------------------------------------------------
hex_pair_to_byte proc
    ; For letter, it will be - 97 ('a') + 10 (11th letter counting from 0)
    ; - 48 (after that comes another substraction for decimal digits) in the
    ; first substraction.
    cmp     ah, 'a'
    jl      process_first_digit
    sub     ah, 39d
    process_first_digit:
    sub     ah, 48d

    cmp     al, 'a'
    jl      process_second_digit
    sub     al, 39d
    process_second_digit:
    sub     al, 48d

    ; Put those two together.
    shl     ah, 4
    add     al, ah

    ; Reset AH so whole AX contains only the converted number.
    xor     ah, ah
    ret
hex_pair_to_byte endp

;------------------------------------------------------------------------------
; MOVE_LEFT
;------------------------------------------------------------------------------
move_left proc
    push    ax
    push    cx ; Used by modulo procedure.
    push    bx ; Different value will be needed there for a while.

    mov     al, bl
    mov     cl, 17
    call    modulo

    ; If current position mod 17 is equal to 0 it means bishop is next to the
    ; left edge.
    mov     al, ah
    cmp     ax, 0
    je      escape_move_left

    pop     bx
    dec     bx
    ; It is simpler (and maybe faster) to put it there again instead
    ; of handling if-like instruction to check whether it was already popped
    ; from the stack or not.
    push    bx

    escape_move_left:
    pop     bx
    pop     cx
    pop     ax
    ret
move_left endp

;------------------------------------------------------------------------------
; MOVE_TOP
;------------------------------------------------------------------------------
move_top proc
    ; If bishop is in the first row it can't move to the top.
    cmp     bx, 17
    jl      escape_move_to
    sub     bx, 17
    escape_move_to:
    ret
move_top endp

;------------------------------------------------------------------------------
; MOVE_RIGHT
;------------------------------------------------------------------------------
move_right proc
    push    ax
    push    cx ; Used by modulo procedure.
    push    bx ; Different value will be needed there for a while.

    mov     al, bl
    mov     cl, 17
    call    modulo

    ; Check if current position mod 17 is 16. If yes, it means bishop is
    ; exactly at the right edge and can't move any further.
    mov     al, ah
    xor     ah, ah
    cmp     ax, 16
    je      escape_move_right

    pop     bx
    inc     bx
    push    bx ; Same as in move_left.

    escape_move_right:
    pop     bx
    pop     cx
    pop     ax
    ret
move_right endp

;------------------------------------------------------------------------------
; MOVE_BOTTOM
;------------------------------------------------------------------------------
move_bottom proc
    ; Bishop is in the last row so it can't move to the bottom.
    cmp     bx, 135
    jg      escape_move_bottom
    add     bx, 17
    escape_move_bottom:
    ret
move_bottom endp

;------------------------------------------------------------------------------
; PROCESS_ONE_BYTE
; Does 4 bishop moves based on AX.
; Changes cell id provided in BX.
;------------------------------------------------------------------------------
process_one_byte proc
    push    ax ; Source for moves.
    push    cx ; Loop counter.

    mov     cx, 4
    make_moves:
    shr     ax, 1
    jc      right

    shr     ax, 1
    jc      move_bottom_left
    jmp     move_top_left

    jmp     next ; Skip second part of move detection if first half matched.

    right:
    shr     ax, 1
    jc      move_bottom_right
    jmp     move_top_right

    next:
    inc     ds:[chessboard+bx]
    loop    make_moves

    pop     cx
    pop     ax
    ret

    ; Code below takes into consideration version number. 1 means that bishop
    ; will try to move 2 cells in the direction he is sent to by fingerprint.
    move_top_left:
    call    move_top
    call    move_left

    cmp     ds:[args], 0
    je      next

    call    move_top
    call    move_left
    jmp     next

    move_top_right:
    call    move_top
    call    move_right

    cmp     ds:[args], 0
    je      next

    call    move_top
    call    move_right
    jmp     next

    move_bottom_left:
    call    move_bottom
    call    move_left

    cmp     ds:[args], 0
    je      next

    call    move_bottom
    call    move_left
    jmp     next

    move_bottom_right:
    call    move_bottom
    call    move_right

    cmp     ds:[args], 0
    je      next

    call    move_bottom
    call    move_right
    jmp     next

process_one_byte endp

;------------------------------------------------------------------------------
; MOVE_BISHOP
; Handles moves for a byte given in AX. BX is left with last position.
;------------------------------------------------------------------------------
move_bishop proc
    push    ax ; General calculations.
    push    di ; Pointer in args array.

    mov     bx, 76 ; Start in the middle of chessboard...
    mov     di, 1 ; ... and at the first hex digit (version number occupies 0).
    mov     cx, 16

    moves:
    mov     ah, ds:[args+di]
    mov     al, ds:[args+di+1]
    add     di, 2
    call    hex_pair_to_byte
    call    process_one_byte
    loop    moves

    pop     di
    pop     ax
    ret
move_bishop endp

;------------------------------------------------------------------------------
; CONVERT_VISITS_TO_ASCII
; Iterates over `chessboard` and translates number of visits to ASCII symbol
; according to `symbols` array. Starts with BX containing last position.
;------------------------------------------------------------------------------
convert_visits_to_ascii proc
    push    ax ; General calculations.
    push    si ; Number of visits in each cell.
    push    di ; Index in chessboard.

    xor     ax, ax
    xor     di, di

    print_cell:
    mov     al, ds:[chessboard+di]
    mov     si, ax

    ; Do max(num_of_visits, 14).
    cmp     si, 14
    jle     convert_char
    mov     si, 14

    convert_char:
    mov     al, ds:[symbols+si]
    mov     ds:[chessboard+di], al

    ; Loop over cells of array.
    inc     di
    cmp     di, 153
    jl      print_cell

    mov     ds:[chessboard+76], 'S'
    mov     si, bx
    mov     ds:[chessboard+si], 'E'

    pop     di
    pop     si
    pop     ax
    ret
convert_visits_to_ascii endp

;------------------------------------------------------------------------------
; PRINT_ASCII_ART
; Print result of program to the console.
;------------------------------------------------------------------------------
print_ascii_art proc
    push    ax ; General calculations.
    push    cx ; Counter in loop.
    push    si ; Index of cell in chessboard.

    ; Print top border.
    mov     si, offset border_top
    call    print_str
    call    new_line

    mov     cx, 9
    xor     si, si
    print_rows:

    ; Add left border.
    mov     al, ds:[border_side]
    call    print_char

    ; Inner loop: (17 columns)
    push    cx
    mov     cx, 17
    print_cells:
    xor     ax, ax
    mov     al, ds:[chessboard+si]
    call    print_char
    inc     si
    loop    print_cells

    ; Add right border.
    mov     al, ds:[border_side]
    call    print_char
    call    new_line

    ; Restore CX's value from outer loop.
    pop     cx
    loop    print_rows

    ; Print bottom border.
    mov     si, offset border_bottom
    call    print_str

    pop     si
    pop     cx
    pop     ax
    ret
print_ascii_art endp

;------------------------------------------------------------------------------
; CLOSE
; Exits to DOS.
;------------------------------------------------------------------------------
close proc
    mov     ah, EXIT
    int     DOS
close endp

start:
    mov     ax, seg data
    mov     ds, ax

    mov     sp, offset peak
    mov     ax, seg peak
    mov     ss, ax

    call    read_args
    call    parse_args
    call    move_bishop
    call    convert_visits_to_ascii
    call    print_ascii_art
    call    close
code ends

stack segment stack
    dw      100h dup (0)
    peak    dw ?
stack ends

end start