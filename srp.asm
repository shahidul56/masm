.387
; ASCII:
TAB             equ 9
SPACE           equ 32
CR              equ 13
LF              equ 10

; Consts:
CHAR_PRINT      equ 2h
LINE_PRINT      equ 9h
ARGS_LENGTH     equ 80h
ARGS_START      equ 82h

; Program controls:
EXIT            equ 4ch
GET_KEYSTROKE   equ 16h

DOS             equ 21h

; Params:
MAX_ARGS_SIZE   equ 32
MAX_ARGS_COUNT  equ 2
ARG_SEPARATOR   equ '$'

assume ss:stack, ds:data , cs:code
data segment
    ; All non-whitespace characters from args that are stored in memory.
    args                    db MAX_ARGS_SIZE dup (0)
    ; Array containing lengths of args.
    args_lengths            db MAX_ARGS_COUNT dup (0)
    ; Total count of used arguments.
    args_count              db 0
    
    iternum                 db 0
    len                     db 0
    
    ; Both inclusive.
	x_max_pos	    		equ 27fh
	y_max_pos   			equ 1dfh ;640x480
	
    x_pos	    			dw ?
	y_pos   				dw 1dfh
	deg_delta				dw 60d
	deg_radian				dw 180d
    
    ; Errors:
    err_general_info        db 'Program expects two arguments: number of iterations and triangle edge in pixels (0-255).$'
    err_too_big_num         db 'Provided number was too big to be stored in memory.',CR,LF,'$'
    err_too_long            db 'Total length of provided arguments was too long (buffer overflow).',CR,LF,'$'
    err_too_many_args       db 'Too many arguments provided.',CR,LF,'$'
    err_wrong_args          db 'Incorrect arguments.',CR,LF,'$'
    err_invalid_char        db 'Only numbers allowed.',CR,LF,'$'
    
    ; Used by `print_num` procedure.
    num_buffer    	db 6 dup ('$')
    num_buffer_size         db 0
data ends

code segment

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
; ARG_TO_NUM
; Convert number from string in `args`.
; Takes index of arg (0 based) in AX, returns number in AX.
;------------------------------------------------------------------------------
arg_to_num proc
    ; AX - multiplier.
    push    bx ; Offset.
    push    cx ; Counter.
    push    dx ; Result.

    xor     bx, bx
    xor     cx, cx
    xor     dx, dx

    push    ax
    ; Count offset of given arg and put it in BX.
    cmp     ax, 0
    je      get_length ; First argument so offset is 0 so bx can stay 0.
    
    mov     cx, ax
    xor     ax, ax
    count_offset:
    push    bx
    mov     bl, cl
    dec     bl
    mov     al, ds:[args_lengths+bx]
    pop     bx
    inc     al ; Separator.
    add     bl, al
    loop    count_offset
    
    ; Get length of the argument and store it in CX.
    get_length:
    pop     ax
    push    bx
    mov     bx, ax
    mov     al, ds:[args_lengths+bx]
    pop     bx
    mov     cx, ax
    
    ; Put digits on stack.
    mov     dx, 0
    put_digit_on_stack:
    mov     al, ds:[args+bx]
    push    ax
    inc     dx
    inc     bx
    cmp     dx, cx
    jl      put_digit_on_stack
    
    ; Loop on the stack.
    xor     ax, ax ; Current digit.
    mov     bx, 1 ; Multiplier.
    mov     dx, 0 ; Result;
    
    process_digit:
    pop     ax
    
    sub     ax, '0'
    push    dx
    mul     bx ; AX * BX -> DX * 2^16 + AX
    pop     dx
    
    add     dx, ax
    mov     ax, 10
    push    dx
    mul     bx
    mov     bx, ax
    pop     dx
    loop    process_digit

    mov     ax, dx ; Return result in AX.
    
    pop     dx
    pop     cx
    pop     bx
    ret
arg_to_num endp

;------------------------------------------------------------------------------
; FLUSH_ARGS
; Increases count of args, puts length of last argument in array and resets CX
; which stores count of characters in currently processed argument.
;------------------------------------------------------------------------------
flush_arg proc
    push    ax
    push    bx
    
    ; Add separator.
    mov     ax, ARG_SEPARATOR
    mov     ds:[di], ax
    inc     di
    
    xor     ax, ax
    xor     bx, bx

    ; Set length of last arg.
    mov     bl, ds:[args_count]
    mov     ds:[args_lengths+bx], cl

    ; Increase argument count.
    mov     al, ds:[args_count]
    inc     al
    mov     ds:[args_count], al

    xor     cx, cx ; Set counter of characters in arg to 0.
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

    xor     bx, bx
    add     bx, 1 ; We don't count last separator so we can start with offset.
    xor     cx, cx
    mov     si, ARGS_START
    mov     di, offset args

    read_chars:
    ; Check if amount of non-whitespace characters exceeds buffer.
    ; If yes, exit with error.
    cmp     bx, MAX_ARGS_SIZE
    jg      catch_err_too_long

    ; Get next char.
    mov     al, es:[si]
    
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

    ; If it is something other than letter, return error.
    cmp     al, '0'
    jl      catch_err_invalid_char
    cmp     al, '9'
    jg      catch_err_invalid_char
    
    ; If user already provided maximum number of arguments...
    push    ax
    mov     al, MAX_ARGS_COUNT
    cmp     al, ds:[args_count]
    pop     ax
    je      catch_err_too_many_args

    ; Else, proceed storing in memory.
    mov     dl, al
    mov     ds:[di], dl
    inc     di
    inc     bx
    inc     cx
    jmp     inc_iterator

    found_whitespace:
    ; If CX is not equal to 0 it means an argument just ended.
    cmp     cx, 0
    je      inc_iterator

    ; Update info about arguments in memory.
    call    flush_arg

    inc_iterator:
    inc     si
    jmp     read_chars

    ; If user provided at least one arg CX won't be equal to 0 and last arg
    ; should be parsed like others. If not, jump straight to exit.
    check_last_arg:
    cmp     cx, 0
    je      end_of_args

    call    flush_arg
    inc     bx ; Increase total amount of characters by separator.
    
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
    
    catch_err_invalid_char:
    mov     si, offset err_invalid_char
    call    exit_err
read_args endp

;------------------------------------------------------------------------------
; PARSE_ARGS
; Checks if given args follow one of two formats:
; '-v input' or 'input output' where input is a file that can be opened and
; output is legal path for a new file.
;------------------------------------------------------------------------------
parse_args proc
    push    ax ; General calculations.
    push    bx
    push    cx ; Loop counter.
    push    di ; Pointer in array.
    
    ; Check if user provided *exactly* two arguments.
    xor     ax, ax
    mov     al, ds:[args_count]
    cmp     ax, 2
    jl      catch_err_wrong_args
    
    ; Check if both args are possible to store in AX (length is less than 5).
    xor     ax, ax
    mov     al, ds:[args_lengths]
    cmp     ax, 5
    jg      catch_err_too_big_num
    
    xor     ax, ax
    mov     al, ds:[args_lengths+1]
    cmp     ax, 5
    jg      catch_err_too_big_num
    
    return_parse_args:
    pop     di
    pop     cx
    pop     bx
    pop     ax
    ret
    
    catch_err_wrong_args:
    mov     si, offset err_wrong_args
    call    exit_err
    
    catch_err_too_big_num:
    mov     si, offset err_too_big_num
    call    exit_err
parse_args endp

;------------------------------------------------------------------------------
; L_SYSTEM_B
; Recursively generate strings in L-system.
; Changes A to B-A-B.
; AX stores number of iterations left.
;------------------------------------------------------------------------------ 	
l_system_a proc
    push    ax
    cmp     ax, 0
    je      draw_a
    
    dec     ax
    call    l_system_b ; B
    call    sub_angle  ; - 
    call    l_system_a ; A
    call    sub_angle  ; -
    call    l_system_b ; B
    jmp     ret_l_system_a
    
    draw_a:
    call    draw_edge  ; B
    call    sub_angle  ; -
    call    draw_edge  ; B
    call    sub_angle  ; -
    call    draw_edge  ; B
    
    ret_l_system_a:
    pop     ax
    ret
l_system_a endp
	
;------------------------------------------------------------------------------
; L_SYSTEM_B
; Recursively generate strings in L-system.
; Changes B to A+B+A.
; AX stores number of iterations left.
;------------------------------------------------------------------------------ 	
l_system_b proc
    push    ax
    cmp     ax, 0
    je      draw_b
    
    dec     ax
    call    l_system_a	; A
    call    add_angle	; +
    call    l_system_b	; B
    call    add_angle	; +
    call    l_system_a	; A
    jmp     ret_l_system_b
    
    draw_b:
    call    draw_edge   ; A
    call    add_angle   ; +
    call    draw_edge   ; A
    call    add_angle   ; +
    call    draw_edge   ; A
    
    ret_l_system_b:
    pop     ax
    ret
l_system_b endp

;------------------------------------------------------------------------------
; INIT_FPU
; Initialize stack of the FPU.
;------------------------------------------------------------------------------     
init_fpu proc
    finit
    fild    ds:[deg_radian]
    fild    ds:[deg_delta]
    fdiv    st(0), st(1) ; st(0) <= 60/180
    fldpi	             ; st(0) <= π, st(1) <= 1/3
    fmul    st(0), st(1) ; st(0) <= 1/3 * π
    fldz				 ; st(0) <= 0 st(1) <= 1/3 * π
    fldz                 ; st(0) <= 0 st(1) <= 0 st(2) <= 1/3 * π
    fldz                 ; st(0) <= 0 st(1) <= 0 st(2) <= 0 st(3) <= 1/3 * π
    ; st(0) <= x_pos
    ; st(1) <= y_pos
    ; st(2) <= angle
    ; st(3) <= delta (1/3 * π)
    ret
init_fpu endp

;------------------------------------------------------------------------------
; GRAPHICS_INIT
; Initialize 640x480 B/W graphics (MCGA,VGA)
;------------------------------------------------------------------------------
graphics_init proc
    push    ax
    xor     ax,ax
    mov     al, 11h ; 640x480 B/W graphics (MCGA,VGA)
    int     10h
    
    pop     ax
    ret
graphics_init endp

;------------------------------------------------------------------------------
; TEXT_MODE
; Exit graphics mode and go back to text mode 80x25.
;------------------------------------------------------------------------------	
text_mode proc
    push    ax
    mov     ah, 0
    int     GET_KEYSTROKE
    mov     al, 3
    mov     ah, 0
    int     10h
    pop     ax 
    ret
text_mode endp
	
;------------------------------------------------------------------------------
; ADD_ANGLE
; Add 1/3π.
;------------------------------------------------------------------------------		
add_angle proc
    fxch    st(2)        ; st(0) <=> st(2)
    fadd    st(0), st(3) ; st(0) <= angle + 1/3 * π
    fxch    st(2)        ; st(0) <=> st(2)
    ret
add_angle endp
	
;------------------------------------------------------------------------------
; SUB_ANGLE
; Substract 1/3 π.
;------------------------------------------------------------------------------  	
sub_angle proc
    fxch    st(2)
    fsub    st(0), st(3)
    fxch    st(2)
    ret
sub_angle endp

;------------------------------------------------------------------------------
; SINCOS
; Calculate sin and cos and leave them on the stack.
;------------------------------------------------------------------------------	
sincos proc
    fld     st(2) ; st(0) <= st(2) (angle)
    fsincos       ; st(0) <= cos(a) st(1) <= sin(a)
    fxch    st(2) ; cos(a) <=> x
    fxch    st(1) ; x <=> sin(a)
    fxch    st(3) ; y <=> sin(a)
    fxch    st(1) ; y <=> x
    ; st(0) <= x st(1) <= y st(2) <= cos(a)
    ; st(3) <= sin(a) st(4) <= a st(5) <= π
    ret
sincos endp

;------------------------------------------------------------------------------
; REVERSE_SINCOS
; Reverse effect of SINCOS procedure.
;------------------------------------------------------------------------------	
reverse_sincos proc
    ; Starts with:
    ; st(0) <= x st(1) <= y st(2) <= cos(a)
    ; st(3) <= sin(a) st(4) <= a st(5) <= π
    fxch    st(2) ; st(0) <= cos(a) st(2) <= x
    fstp    st(0) ; <<
    fxch    st(2) ; st(2) <= y st(0) <= sin(a)
    fstp    st(0) ; <<
    ; st(0) <= x st(1) <= y st(2) <= a st(3) <= 1/3 * π
    ret
reverse_sincos endp

;------------------------------------------------------------------------------
; DRAW_PIXEL
; Does necessary calculations and draws pixel to the screen if its position is
; within borders.
;------------------------------------------------------------------------------	
draw_pixel proc
    push    ax
    push    cx
    push    dx
    
    ; Starts with:
    ; st(0) <= x st(1) <= y st(2) <= cos(a)
    ; st(3) <= sin(a) st(4) <= a st(5) <= π
    fadd    st(0), st(2)           ; st(0) <= x + cos(a)
    fist    word ptr x_pos         ; x_pos <= int(st(0))
    fxch    st(1)                  ; x + cos(a) <=> y
    fadd    st(0), st(3)           ; st(0) <= y + sin(a)
    fld     st(0)				   ; st(0) <= st(0) (copy())
    fabs						   ; st(0) <= abs(st(0))
    fistp   word ptr y_pos		   ; y_pos <= int(st(0)); pop()
    fxch    st(1)                  ; y + sin(a) <=> x + cos(a)
    mov     cx, word ptr x_pos
    mov     ax, word ptr y_max_pos
    sub     ax, word ptr y_pos
    mov     dx, ax
    
    ; Make border checks.
    cmp     cx, 0
    jl      ret_draw_pixel
    cmp     cx, x_max_pos
    jg      ret_draw_pixel
    cmp     dx, 0
    jl      ret_draw_pixel
    cmp     dx, x_max_pos
    jg      ret_draw_pixel
    
    ; If passed, draw pixel.
    mov     ah, 0ch	; Write pixel.
    mov     al, 02h	; Color.
    int     10h
    
    ret_draw_pixel:
    pop     dx
    pop     cx
    pop     ax
    ret	
draw_pixel endp
	
;------------------------------------------------------------------------------
; DRAW_EDGE
; Draw one edge of triangle.
;------------------------------------------------------------------------------
draw_edge proc
    push    cx
    
    xor     cx,cx
    mov     cl, len
    
    call    sincos
    
    draw_pixels:
    call    draw_pixel
    loop    draw_pixels
    
    call    reverse_sincos
    
    pop cx
    ret
draw_edge endp

;------------------------------------------------------------------------------
; DRAW_SIERPINSKI
; Starting point of drawing.
;------------------------------------------------------------------------------
draw_sierpinski proc
    push    ax
    xor     ax, ax
    mov     al, iternum
    
    call    graphics_init
    call    init_fpu
    
    cmp     al, 0
    je      no_iterations
    
    dec     al
    call    l_system_a
    jmp     ret_draw_sierpinski
    
    no_iterations:
    call    draw_edge
    
    ret_draw_sierpinski:
    call text_mode
    
    pop ax
    ret
draw_sierpinski endp

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
    
    mov     ax, seg peak
    mov     ss, ax
    mov     sp, offset peak
    
    call    read_args
    call    parse_args
    
    mov     ax, 0
    call    arg_to_num
    mov     ds:[iternum], al
    
    mov     ax, 1
    call    arg_to_num
    mov     ds:[len], al
    
    call    draw_sierpinski
    call    close
code ends

stack segment stack
    dw      100h dup (?)
    peak    dw ?
stack ends

end start