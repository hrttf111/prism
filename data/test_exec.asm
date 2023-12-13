BITS 16
CPU 8086

%macro save_regs 0
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    push bp
    push sp
%endmacro

%macro load_regs 0
    pop sp
    pop bp
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
%endmacro

%macro save_regs 1
    mov WORD [%1], ax
    mov WORD [%1+2], cx
    mov WORD [%1+4], dx
    mov WORD [%1+6], bx
    mov WORD [%1+8], sp
    mov WORD [%1+10], bp
    mov WORD [%1+12], si
    mov WORD [%1+14], di
    mov WORD [%1+32], es
    mov WORD [%1+34], cs
    mov WORD [%1+36], ss
    mov WORD [%1+38], ds
%endmacro

REG_AREA equ 0x7E04

;Bootloader

SECTION bootloader start=0
mov ax, 0
mov ss, ax
mov sp, 0x7C00
sti
;
;read disk
mov bx, 0
mov ax, 1000h
mov es, ax
mov ah, 2
mov al, 3   ; number of sectors
mov ch, 3   ; track
mov cl, 3   ; sector
mov dh, 1   ; head
mov dl, 0   ; drive
int 0x13
mov [0x7E00], al
mov [0x7E01], ah
;jmp 1000h:START
jmp far [START]

STOP:
;save_reg REG_AREA
mov WORD [magic_num], 0xEE88
mov WORD [magic_num+2], 0xFF99
FINISHED:
    sti
    hlt
    jmp FINISHED
times 510-($-$$) db 0
dw 0xAA55

absolute 0x7E00
magic_num resw 2

;Test section

SECTION .text start=10000h
START:
    jmp 0:STOP
