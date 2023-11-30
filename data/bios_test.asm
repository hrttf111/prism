BITS 16
CPU 8086

%macro print 1
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    mov cx, 255
    mov di, %1
    mov si, di
    call strl
    call print_str
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
%endmacro

SECTION bootloader start=7C00h
;mov sp, 0xf00
;mov ss, sp
mov sp, 0
mov ss, sp
mov sp, 0x7C00
;
mov bx, 0x7C00
mov ax, 0
mov es, ax
mov ah, 2
mov al, 255 ; number of sectors
mov ch, 3   ; track
mov cl, 9   ; sector
mov dh, 0   ; head
mov dl, 1   ; drive
int 0x13
;mov al, 10
jmp 1000h:START
times 510-($-$$) db 0
dw 0xAA55

SECTION other start=8000h
str2 db "2322",0
str_test db "1234567890abcdef",0
str_int db "Timer interrupt",0
str_end db "End",0
str_empty db 0

SECTION .data start=9000h
str1 db "11110---112333",0
numbers db "0123456789abcdef",0

absolute 0x9200
counter_1   resw    1
int_counter resw    2
temp_str    resb    16
key_str     resb    4

SECTION .text start=10000h
strl:
    mov ax, 0
    mov cx, 255
    mov dx, di
    repnz scasb
    mov cx, di
    jcxz STRL_END
    sub cx, dx
    dec cx
STRL_END:
    ret

print_str:
    ; SI - source str
    ; CX - str length
    push ax
    jcxz PRINT_END
PRINT_CHAR:
    mov ah, 0xe
    mov al, BYTE [si]
    int 0x10
    inc si
    loop PRINT_CHAR
PRINT_END:
    pop ax
    ret

num_to_str:
    ; ax - num
    mov di, temp_str
NUM_LOOP:
    cmp ax, 0
    je NUM_TO_STR_END
    mov bx, ax
    and bh, 0xf0
    mov cl, 12
    shr bx, cl
    mov dl, [numbers + bx]
    mov [di], dl
    inc di
    mov cl, 4
    shl ax, cl
    jmp NUM_LOOP
NUM_TO_STR_END:
    mov BYTE [di], 0
    ret

test_scroll:
    push ax
    push bx
    push cx
    push dx
    mov ah, 6 ; up
    ;mov ah, 7 ; down
    mov al, 1 ; distance
    mov bh, 0 ; attr
    mov ch, 0 ; top
    mov cl, 3 ; left
    mov dh, 5 ; bottom
    mov dl, 20 ; right
    int 0x10
    pop dx
    pop cx
    pop bx
    pop ax
    ret

START:
cli
;
mov WORD [int_counter], 0
;Configure PIC and PIT
PIC1  equ  0x20
PIC1D equ  0x21
ICW1  equ  0x17
ICW2  equ  0x08 ; Master interrupts 0x20-0x27
ICW3  equ  0x00
ICW4  equ  0x03
OCW1  equ  0x00
;Init Master
mov al, ICW1
out PIC1, al
mov al, ICW2
out PIC1D, al
mov al, ICW3
out PIC1D, al
mov al, ICW4
out PIC1D, al
mov al, OCW1
out PIC1D, al
;Init PIT
PIT_REG_COUNTER0 equ 0x40
PIT_REG_COMMAND  equ 0x43
mov ax, 0
mov es, ax
mov bx, cs
mov [es:0x1c * 4], WORD TIMER_INT
mov [es:0x1c * 4+2], bx
PIT_READ_STATUS  equ 0xE2 ; ReadBack Timer0, Status
;PIT_COMMAND      equ 0x30 ; Timer0, Mode0, 2 Bytes, HEX
PIT_COMMAND      equ 0x34 ; Timer0, Mode2, 2 Bytes, HEX
push ax
mov al, PIT_COMMAND
out PIT_REG_COMMAND, al
pop ax
;;
sti
;;
mov ax, 0x7138
call num_to_str
print temp_str
;;
xor bx, bx
xor dx, dx
mov cx, 90
push ax
mov al, 10 ; least
out PIT_REG_COUNTER0, al
;mov al, 0 ; most
mov al, 10 ; most
out PIT_REG_COUNTER0, al
pop ax
;;;;
;Main app
mov ah, 0xe
mov al, 'a'
int 0x10
mov ah, 0xe
mov al, 'b'
int 0x10
;
print str2
print str_empty
print str1

mov cx, 60
LOOPSTR:
    print str_test
    print str1
    loop LOOPSTR
;
mov WORD [counter_1], 100
ELOOPS:
sti
mov cx, 60000
ELOOP:
    loop ELOOP
cli
mov cx, WORD [counter_1]
dec cx
mov WORD [counter_1], cx
loop ELOOPS
sti
;;
call test_scroll
cli
mov cx, 60000
ELOOP1:
    loop ELOOP1
sti
;;
LOOP_INF:
    mov cx, 10
    loop LOOP_INF
print str_end
mov ax, 0
mov ax, WORD [int_counter]
mov bx, WORD [counter_1]
mov dx, 0
hlt

TIMER_INT:
push cx
mov ax, WORD [int_counter]
inc ax
mov WORD [int_counter], ax
call num_to_str
print temp_str
;
mov ah, 1
int 0x16
jz END_INT
mov ah, 0
int 0x16
mov BYTE [key_str], 0x20
mov BYTE [key_str+1], al
mov BYTE [key_str+2], 0x20
mov BYTE [key_str+3], 0
print key_str
;
END_INT:
pop cx
iret
