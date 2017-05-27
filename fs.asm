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

; File operations:
CREATE_FILE     equ 3ch
OPEN_FILE       equ 3dh
CLOSE_FILE      equ 3eh
READ_FILE       equ 3fh
WRITE_FILE      equ 40h
DELETE_FILE     equ 41h

; Program controls:
EXIT            equ 4ch
DOS             equ 21h

; Params:

; In this project:
; 1 byte for version
; 32 bytes for ASCII hex
MAX_ARGS_SIZE   equ 128
MAX_ARGS_COUNT  equ 2
ARG_SEPARATOR   equ 0
BUFFER_SIZE     equ 16383

data segment
    ; Current offset in buffer (for processing).
    ; Starts at buffer size to call initial reload.
    pointer                 dw BUFFER_SIZE
    ; Buffer for storing files content while processing.
    buffer                  db BUFFER_SIZE dup (0)
    
    ; File handlers:
    input                   dw 0
    output                  dw 0
    
    ; All non-whitespace characters from args that are stored in memory.
    args                    db MAX_ARGS_SIZE dup (0)
    ; Array containing lengths of args.
    args_lengths            db MAX_ARGS_COUNT dup (0)
    ; Total count of used arguments.
    args_count              db 0
    
    ; Print stats:
    whitespace_num          dw 0
    punctuations_num        dw 0
    digits_num              dw 0
    letters_num             dw 0
    words_num               dw 0
    sentences_num           dw 0
    lines_num               dw 0
    
    ; Text messages:
    info                    db 'Given file contains: '
    info_whitespace         db CR,LF,'Whitespace: '
    info_punctuation        db CR,LF,'Punctuation marks: '
    info_digits             db CR,LF,'Digits: '
    info_letters            db CR,LF,'Letters: '
    info_words              db CR,LF,'Words: '
    info_sentences          db CR,LF,'Sentences: '
    info_lines              db CR,LF,'Lines: '
    new_line                db CR,LF
    
    ; Lengths of strings (for file writing):
    info_length             db 21
    info_whitespace_length  db 14
    info_punctuation_length db 21
    info_digits_length      db 10
    info_letters_length     db 11
    info_words_length       db 9
    info_sentences_length   db 13
    info_lines_length       db 9
    
    file_is_correct         db 'Correct!$'
    
    ; Errors:
    err_general_info        db 'Program expects two formats of input:',CR,LF,'"-v input" - check for letters, numbers, whitespace and punctuation marks.',CR,LF,'"input output" - analyses input file and outputs statistics to output file.$'
    err_file_open           db 'Could not open specified file.',CR,LF,'$'
    err_file_create         db 'Could not create a file with specified name.',CR,LF,'$'
    err_file_read           db 'Could not read file.$'
    err_file_write          db 'Could not write to file.$'
    err_file_close          db 'Could not close file.$'
    err_too_long            db 'Total length of provided arguments was too long (buffer overflow).',CR,LF,'$'
    err_wrong_file_1        db 'File contains at least one character that is not alphanumeric, punctuation mark or whitespace (line: $'
    err_wrong_file_2        db ', character: $'
    err_wrong_file_3        db ').$'
    err_unrecognized_char   db 'Unrecognized character occured. Cannot measure statistics of the file.$'
    err_out_of_disk_space   db 'Out of disk space. Cannot write output file.$'
    err_too_many_args       db 'Too many arguments provided.',CR,LF,'$'
    err_wrong_args          db 'Incorrect arguments.',CR,LF,'$'
    
    ; Used by `print_num` procedure.
    num_buffer    	db 6 dup ('$')
    num_buffer_size         db 0
data ends

code segment

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
; NUM_TO_STR
; Converts number from AX to ASCII string in the buffer.
;------------------------------------------------------------------------------
num_to_str proc
    push    ax ; Source.
	push	bx ; Used in division.
	push	cx ; Loop counter.
	push	dx ; Result of division.
    push    si ; Text pointer.
    
	mov 	bx, 10
	mov 	cx, 0

	to_stack:
	mov 	dx, 0
	div 	bx
	push 	dx
	inc		cx
	cmp 	ax, 0
	jne 	to_stack
    
    mov     ds:[num_buffer_size], cl
	mov 	si, offset num_buffer

	from_stack:
	pop 	dx
	add 	dl, '0'
	mov 	ds:[si], dl
	inc		si
	loop	from_stack

    pop     si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	ret
num_to_str endp

;------------------------------------------------------------------------------
; PRINT_NUM
; Prints number given in AX.
;------------------------------------------------------------------------------
print_num proc
	push 	ax ; Source.
	push	bx ; Used in division.
	push	cx ; Loop counter.
    
    ; Clear buffer before printing.
    mov     cx, 5
    clear_buffer:
    mov     bx, cx
    mov     ds:[num_buffer+bx], '$'
    loop    clear_buffer
    
    call    num_to_str

    mov     si, offset num_buffer
    call    print_str
    
	pop		cx
	pop		bx
	pop		ax
	ret
print_num endp

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
; FILE_CREATE
; Creates file with filename's offset taken from SI and returns file handle in
; AX. Prints error if operation was not possible.
;------------------------------------------------------------------------------
file_create proc
    push    cx ; File attributes.
    push    dx ; Gives int 21h function filename.
    
    xor     cx, cx ; Normal file creation, no attributes.
    mov     ah, CREATE_FILE
    mov     dx, si
    int     DOS
    jnc     return_file_create
    
    ; Error.
    mov     ax, 0
    
    return_file_create:
    pop     dx
    pop     cx
    ret
file_create endp

;------------------------------------------------------------------------------
; FILE_OPEN
; Opens file with filename's offset taken from SI and returns file handle in
; AX if file was opened successfully, otherwise returns 0.
; Prints error if operation was not successful.
;------------------------------------------------------------------------------
file_open proc
    push    cx ; File attributes.
    push    dx ; Gives int 21h function filename.
    
    mov     al, 0
    mov     ah, OPEN_FILE
    mov     dx, si
    int     DOS
    jnc     return
    
    ; Error.
    mov     ax, 0
    
    return:
    pop     dx
    pop     cx
    ret
file_open endp

;------------------------------------------------------------------------------
; FILE_CLOSE
; Closes file referenced by handle in AX.
; Prints error if operation was not successful.
;------------------------------------------------------------------------------
file_close proc
    push    ax
    ; No file handler - nothing to close.
    cmp     ax, 0
    je      return_file_close
    
    mov     bx, ax
    mov     ah, CLOSE_FILE
    int     DOS
    jc      catch_err_file_close
    
    return_file_close:
    pop     ax
    ret
    
    catch_err_file_close:
    mov     si, offset err_file_close
    call    print_str
    call    close
file_close endp

;------------------------------------------------------------------------------
; FILE_WRITE
; Writes number of bytes specified in AX from SI to ds:[output].
; Prints error if operation was not successful.
;------------------------------------------------------------------------------
file_write proc
    push    ax
    push    bx
    push    cx
    push    dx
    
    mov     cx, ax
    mov     ah, WRITE_FILE
    mov     bx, ds:[output]
    cmp     bx, 0
    je      return_file_write
    mov     dx, si
    int     DOS
    
    jc      catch_err_file_write
    
    cmp     ax, cx
    jne     catch_err_out_of_disk_space
    
    return_file_write:
    pop     dx
    pop     cx
    pop     bx
    pop     ax
    ret
    
    catch_err_file_write:
    mov     si, offset err_file_write
    call    print_str
    call    close
    
    catch_err_out_of_disk_space:
    mov     si, offset err_out_of_disk_space
    call    print_str
    call    close
file_write endp

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
    inc     bx
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
    add     bx, 2 ; Offset BX to properly measure length.
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
    
    ; Check if first argument is a file that can be opened.
    mov     si, offset args
    call    file_open
    cmp     ax, 0
    je      check_another_first_arg
    
    ; file_open returned file handler so it should be stored in ds:[input].
    mov     ds:[input], ax
    
    ; First argument was a file so second one is a name for an output file.
    mov     si, offset args
    mov     bl, ds:[args_lengths]
    add     si, bx
    inc     si ; Separator.
    
    call    file_create
    cmp     ax, 0
    je      catch_err_file_create
    mov     ds:[output], ax
    
    jmp     return_parse_args
    
    ; Check if length of first argument is exactly 2 ('-v').
    check_another_first_arg:
    mov     al, ds:[args_lengths]
    cmp     ax, 2
    jne     catch_err_wrong_args
    
    ; Compare first and second character to '-v'.
    mov     al, ds:[args]
    cmp     al, '-'
    jne     catch_err_wrong_args
    mov     al, ds:[args+1]
    cmp     al, 'v'
    jne     catch_err_wrong_args
    
    ; First argument was '-v' so second one is file to check.
    mov     si, offset args
    mov     bl, ds:[args_lengths]
    add     si, bx
    inc     si
    
    call    file_open
    cmp     ax, 0
    je      catch_err_file_open
    
    mov     ds:[input], ax
    
    return_parse_args:
    pop     di
    pop     cx
    pop     bx
    pop     ax
    ret
    
    catch_err_wrong_args:
    mov     si, offset err_wrong_args
    call    exit_err
    
    catch_err_file_create:
    mov     si, offset err_file_create
    call    exit_err
    
    catch_err_file_open:
    mov     si, offset err_file_open
    call    exit_err
parse_args endp

;------------------------------------------------------------------------------
; RELOAD_BUFFER
; Reloads buffer with another part of file. Returns number of read characters.
; 0 means end of file.
;------------------------------------------------------------------------------
reload_buffer proc
    push    bx
    push    cx
    push    dx
    
    ; Read content of the file.
    mov     ax, ds:[input]
    mov     bx, ax
    mov     cx, BUFFER_SIZE
    mov     dx, offset buffer
    mov     ah, READ_FILE
    int     DOS
    jc      catch_err_file_read
    
    ; Reset current pointer.
    mov     ds:[pointer], 0
    
    ; If AX is different from CX it means end of file occured.
    cmp     ax, cx
    je      return_reload_buffer
    
    ; Separate those last bytes of the file from previous buffer content.
    mov     bx, ax
    mov     ds:[buffer+bx], 0
        
    return_reload_buffer:
    pop     dx
    pop     cx
    pop     bx
    ret
    
    catch_err_file_read:
    mov     si, offset err_file_read
    call    print_str
    call    close
reload_buffer endp

;------------------------------------------------------------------------------
; GET_CHAR
; Gets next char from a file. If buffer ends, loads another part.
; Returns character in AX if any, 0 when file ends.
;------------------------------------------------------------------------------
get_char proc
    push    bx
    ; After reading last character buffer might have ended so it would be nice
    ; to reload it for future uses.
    mov     ax, ds:[pointer]
    cmp     ax, BUFFER_SIZE
    jl      read_char_from_memory
    
    call    reload_buffer
    
    ; If file ended return 0.
    cmp     ax, 0
    je      return_get_char
    
    read_char_from_memory:
    mov     bx, ds:[pointer]
    inc     ds:[pointer]
    
    mov     al, ds:[buffer+bx]
    
    return_get_char:
    pop     bx
    ret
get_char endp

;------------------------------------------------------------------------------
; VERIFY_FILE_CONTENT
; First use case of the program. Verifies if the input file contains only
; alphanumeric characters, punctuation marks and whitespace.
;------------------------------------------------------------------------------
verify_file_content proc
    push    ax ; Processing of all characters.
    push    bx ; Current line.
    push    cx ; Current character in line.
    
    mov     bx, 1
    mov     cx, 1
    
    process_file:
    call    get_char
    cmp     ax, 0
    je      return_verify_file_content
    
    ; Legal characters are:
    ; 0-32 whitespace
    ; 33 !
    ; 40-41 ()
    ; 44-46 ,-.
    ; 48-57 [0-9]
    ; 58-59 :;
    ; 63 ?
    ; 65-90 [A-Z]
    ; 97-122 [a-z]
    cmp     al, LF
    jne     compare
    inc     bx
    mov     cx, 0 ; Start from 0 not 1 because LF also increases the counter.
    
    compare:
    cmp     al, ' '
    jle     ok
    cmp     al, '!'
    je      ok
    cmp     al, '('
    je      ok
    cmp     al, ')'
    je      ok
    cmp     al, '+'
    jle     catch_err_wrong_file
    cmp     al, '.'
    jle     ok
    cmp     al, '/'
    jle     catch_err_wrong_file
    cmp     al, ';'
    jle     ok
    cmp     al, '>'
    jle     catch_err_wrong_file
    cmp     al, '?'
    je      ok
    cmp     al, '@'
    je      catch_err_wrong_file
    cmp     al, 'Z'
    jle     ok
    cmp     al, '`'
    jle     catch_err_wrong_file
    cmp     al, 'z'
    jle     ok
    jmp     catch_err_wrong_file
    
    ok:
    inc     cx
    jmp     process_file
    
    return_verify_file_content:
    mov     si, offset file_is_correct
    call    print_str
    
    pop     cx
    pop     bx
    pop     ax
    ret
    
    catch_err_wrong_file:
    mov     si, offset err_wrong_file_1
    call    print_str
    mov     ax, bx
    call    print_num
    mov     si, offset err_wrong_file_2
    call    print_str
    mov     ax, cx
    call    print_num
    mov     si, offset err_wrong_file_3
    call    print_str
    
    call    close
verify_file_content endp

;------------------------------------------------------------------------------
; IS_WHITESPACE
; Check if character given in AL is whitespace (SCII code <= 32).
; Returns to AH 1 if true, 0 otherwise.
;------------------------------------------------------------------------------
is_whitespace proc
    cmp     al, ' '
    jle     whitespace
    jmp     not_whitespace
    
    whitespace:
    mov     ah, 1
    jmp     return_is_whitespace
    
    not_whitespace:
    mov     ah, 0
    
    return_is_whitespace:
    ret
is_whitespace endp

;------------------------------------------------------------------------------
; IS_PUNCTUATION
; Check if character given in AL is punctuation mark [!(),-.:;?].
; Their ASCII codes: 33, 40, 41, 44, 45, 46, 58, 59, 63.
; Returns to AH 1 if punctuation mark, 0 otherwise.
;------------------------------------------------------------------------------
is_punctuation proc
    cmp     al, ' '
    jle     not_punctuation
    cmp     al, '!'
    je      punctuation
    cmp     al, '('
    jl      not_punctuation
    cmp     al, ')'
    jle     punctuation
    cmp     al, ','
    jl      not_punctuation
    cmp     al, '.'
    jle     punctuation
    cmp     al, ':'
    jl      not_punctuation
    cmp     al, ';'
    jle     punctuation
    cmp     al, '?'
    jl      not_punctuation
    je      punctuation
    jmp     not_punctuation
    
    punctuation:
    mov     ah, 1
    jmp     return_is_punctuation
    
    not_punctuation:
    mov     ah, 0

    return_is_punctuation:
    ret
is_punctuation endp

;------------------------------------------------------------------------------
; IS_DIGIT
; Checks if character given in AL is a digit [48-57].
; Returns to AH 1 if letter, 0 otherwise.
;------------------------------------------------------------------------------
is_digit proc
    cmp     al, '0'
    jl      not_digit
    cmp     al, '9'
    jle     digit
    jmp     not_digit
    
    digit:
    mov     ah, 1
    jmp     return_is_digit
    
    not_digit:
    mov     ah, 0
    
    return_is_digit:
    ret
is_digit endp

;------------------------------------------------------------------------------
; IS_LETTER
; Checks if character given in AL is a letter ([65-90] or [97-122]).
; Returns to AH 1 if letter, 0 otherwise.
;------------------------------------------------------------------------------
is_letter proc
    cmp     al, 'A'
    jl      not_letter
    cmp     al, 'Z'
    jle     letter
    cmp     al, 'a'
    jl      not_letter
    cmp     al, 'z'
    jle     letter
    jmp     not_letter
    
    letter:
    mov     ah, 1
    jmp     return_is_letter
    
    not_letter:
    mov     ah, 0
    
    return_is_letter:
    ret
is_letter endp

;------------------------------------------------------------------------------
; PRINT_STATS
; Second use case of the program. Prints how many whitespace, punctuation
; marks, letters, numbers, words, sentences and lines are in the file.
;------------------------------------------------------------------------------
print_stats proc
    push    ax ; Current character.
    push    bx ; 1 if previous character was a letter, 0 otherwise.
    
    ; Start with letter because it can stick to the first word and it won't
    ; mess up calculations.
    mov     bx, 1
    
    calculate_stats:
    call    get_char
    
    cmp     ax, 0
    je      return_print_stats
    
    cmp     al, LF
    jne     check_whitespace
    inc     ds:[lines_num]
    mov     bx, 0
    jmp     continue_not_letter
    
    check_whitespace:
    call    is_whitespace
    cmp     ah, 0
    je      check_punctuation
    inc     ds:[whitespace_num]
    cmp     bx, 0
    je      continue_not_letter
    inc     ds:[words_num]
    jmp     continue_not_letter
    
    check_punctuation:
    call    is_punctuation
    cmp     ah, 0
    je      check_digit
    inc     ds:[punctuations_num]
    cmp     bx, 0
    je      continue_not_letter
    
    ; There must be more specific check for punctuation marks ending sentences.
    cmp     al, '!'
    je      mark_next_sentence
    cmp     al, '?'
    je      mark_next_sentence
    cmp     al, '.'
    je      mark_next_sentence
    cmp     al, ','
    je      mark_comma
    
    jmp     continue_not_letter
    
    mark_next_sentence:
    inc     ds:[words_num]
    inc     ds:[sentences_num]
    jmp     continue_not_letter
    
    mark_comma:
    inc     ds:[words_num]
    jmp     continue_not_letter
    
    check_digit:
    call    is_digit
    cmp     ah, 0
    je      check_letter
    inc     ds:[digits_num]
    jmp     continue_not_letter
    
    check_letter:
    mov     bx, 0
    call    is_letter
    cmp     ah, 0
    je      catch_err_unrecognized_char
    inc     ds:[letters_num]
    
    continue_letter:
    mov     bx, 1
    jmp     continue
    
    continue_not_letter:
    mov     bx, 0

    continue:
    jmp     calculate_stats
    
    return_print_stats:
    mov     si, offset file_is_correct
    call    print_str
    
    mov     si, offset info
    xor     ah, ah
    mov     al, ds:[info_length]
    call    file_write
    
    ; Same for whitespace...
    mov     si, offset info_whitespace
    xor     ah, ah
    mov     al, ds:[info_whitespace_length]
    call    file_write
    
    mov     ax, ds:[whitespace_num]
    call    num_to_str
    mov     si, offset num_buffer
    xor     ah, ah
    mov     al, ds:[num_buffer_size]
    call    file_write
    
    ; ... punctuation...
    mov     si, offset info_punctuation
    xor     ah, ah
    mov     al, ds:[info_punctuation_length]
    call    file_write
    
    mov     ax, ds:[punctuations_num]
    call    num_to_str
    mov     si, offset num_buffer
    xor     ah, ah
    mov     al, ds:[num_buffer_size]
    call    file_write
    
    ; ... digits...
    mov     si, offset info_digits
    xor     ah, ah
    mov     al, ds:[info_digits_length]
    call    file_write
    
    mov     ax, ds:[digits_num]
    call    num_to_str
    mov     si, offset num_buffer
    xor     ah, ah
    mov     al, ds:[num_buffer_size]
    call    file_write
    
    ; ... letters...
    mov     si, offset info_letters
    xor     ah, ah
    mov     al, ds:[info_letters_length]
    call    file_write
    
    mov     ax, ds:[letters_num]
    call    num_to_str
    mov     si, offset num_buffer
    xor     ah, ah
    mov     al, ds:[num_buffer_size]
    call    file_write
    
    ; ... words...
    mov     si, offset info_words
    xor     ah, ah
    mov     al, ds:[info_words_length]
    call    file_write
    
    mov     ax, ds:[words_num]
    call    num_to_str
    mov     si, offset num_buffer
    xor     ah, ah
    mov     al, ds:[num_buffer_size]
    call    file_write
    
    ; ... sentences...
    mov     si, offset info_sentences
    xor     ah, ah
    mov     al, ds:[info_sentences_length]
    call    file_write
    
    mov     ax, ds:[sentences_num]
    call    num_to_str
    mov     si, offset num_buffer
    xor     ah, ah
    mov     al, ds:[num_buffer_size]
    call    file_write
    
    ; ... lines...
    mov     si, offset info_lines
    xor     ah, ah
    mov     al, ds:[info_lines_length]
    call    file_write
    
    mov     ax, ds:[lines_num]
    call    num_to_str
    mov     si, offset num_buffer
    xor     ah, ah
    mov     al, ds:[num_buffer_size]
    call    file_write
    
    ; And final new line:
    mov     si, offset new_line
    xor     ah, ah
    mov     al, 2
    call    file_write
    
    pop     bx
    pop     ax
    ret
    
    catch_err_unrecognized_char:
    ;;;
    call    print_num
    push    ax
    mov     ax, ' '
    call    print_char
    pop     ax
    ;;;
    
    mov     si, offset err_unrecognized_char
    call    print_str
print_stats endp

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
    
    ; If there is no output file program can assume the first version (if there
    ; was an error it would be thrown during parsing.
    cmp     ds:[output], 0
    jne     second_version
    call    verify_file_content
    jmp     cleanup
    
    second_version:
    call    print_stats
    
    cleanup:
    ; There is no need to check if there is file handler for given file because
    ; `file_close` takes care of that.
    mov     ax, ds:[input]
    call    file_close
    mov     ax, ds:[output]
    call    file_close
    
    call    close
code ends

stack segment stack
    dw      100h dup (0)
    peak    dw ?
stack ends

end start