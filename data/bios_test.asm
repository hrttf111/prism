BITS 16
CPU 8086

;%macro print 2
;%endmacro

SECTION bootloader start=7C00h
mov al, 10
jmp 1000h:START
times 510-($-$$) db 0
dw 0xAA55

SECTION other start=1000h
str2 db "2322",0

SECTION .data start=2000h
str1 db "1111",0

SECTION .text start=10000h
func:
    mov cx, 1
    mov si, [str2+1]
    ret

START:
mov ah, 0xe
mov al, 'a'
int 0x10
mov ah, 0xe
mov al, 'b'
int 0x10
mov al, [str1+2]
call func
mov dl, [str2+1]
mov dl, [str2+1]
hlt
