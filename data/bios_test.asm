BITS 16
CPU 8086

%macro print 1
    mov cx, 255
    mov di, %1
    mov si, di
    call strl
    call print_str
%endmacro

SECTION bootloader start=7C00h
mov al, 10
jmp 1000h:START
times 510-($-$$) db 0
dw 0xAA55

SECTION other start=1000h
str2 db "2322",0
str_empty db 0

SECTION .data start=2000h
str1 db "1111",0
numbes db "0123456789\0",0

SECTION .text start=10000h
func:
    mov cx, 1
    mov si, [str2+1]
    ret

strl:
    mov ax, 0
    mov cx, 255
    mov dx, di
    repnz scasb
    mov cx, di
    sub cx, dx
    dec cx
    ret

print_str:
    ; SI - source str
    ; CX - str length
    ; ES - data segment
    push ax
    ;jcxz PRINT_END
PRINT_CHAR:
    mov ah, 0xe
    mov al, BYTE [si]
    int 0x10
    inc si
    loop PRINT_CHAR
;PRINT_END:
    pop ax
    ret

START:
mov ah, 0xe
mov al, 'a'
int 0x10
mov ah, 0xe
mov al, 'b'
int 0x10
;mov ah, 0xe
;mov al, BYTE [str2]
;int 0x10
;
print str2
;print str_empty
print str1
;
;mov dl, [str2+1]
;mov dl, [str2+1]
hlt
