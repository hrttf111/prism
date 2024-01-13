BITS 16
CPU 8086

;------------------------------------------------------------------------------;
;Macros and defines

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

%macro detect_disk 1
    mov dl, 0 ; diskette 0
    mov ah, 8 ; read params
    int 0x13
    ;
    cmp cl, 36
    jne OFF18
    mov [%1], WORD offset1_36
    jmp DETECT_DONE
    OFF18:
    mov [%1], WORD offset1_18
    DETECT_DONE:
%endmacro

%macro read_disk 3
    mov ax, %2
    mov es, ax
    mov ah, 2  ; read
    mov bx, WORD [%1]
    mov al, [bx+BOOTLOADER_START+disk_offset.num_sect]
    mov ch, [bx+BOOTLOADER_START+disk_offset.track]
    mov cl, [bx+BOOTLOADER_START+disk_offset.sector]
    mov dh, [bx+BOOTLOADER_START+disk_offset.head]
    mov dl, 0   ; drive
    mov bx, %3
    int 0x13
    mov [disk_res], al
    mov [disk_res+1], ah
%endmacro

BOOTLOADER_START equ 0x7C00
REG_AREA equ 0x7E10
MAGIC_1 equ 0xEE88
MAGIC_2 equ 0xFF99

struc disk_offset
    .track: resb 1
    .sector: resb 1
    .head: resb 1
    .num_sect resb 1
endstruc

;------------------------------------------------------------------------------;
;Bootloader

SECTION bootloader start=0
mov ax, 0
mov ss, ax
mov sp, BOOTLOADER_START
sti

;read test to memory
detect_disk ptr_offset1
read_disk ptr_offset1, 0x1000, 0

;start test
;cli
jmp 1000h:START

STOP:
mov WORD [magic_num], MAGIC_1
save_regs REG_AREA
mov WORD [magic_num+2], MAGIC_2

FINISHED:
    sti
    hlt
    jmp FINISHED

;------------------------------------------------------------------------------;
;Constants

offset1_18:
    istruc disk_offset
        at disk_offset.track,    db 3
        at disk_offset.sector,   db 3
        at disk_offset.head,     db 1
        at disk_offset.num_sect, db 3
    iend
offset1_36:
    istruc disk_offset
        at disk_offset.track,    db 1
        at disk_offset.sector,   db 21
        at disk_offset.head,     db 1
        at disk_offset.num_sect, db 3
    iend
times 510-($-$$) db 0
dw 0xAA55

;------------------------------------------------------------------------------;
;Vars

absolute 0x7E00
magic_num resw 2
disk_res resb 2
ptr_offset1 resw 1

;------------------------------------------------------------------------------;
;Test section

SECTION .text start=10000h
START:
    mov ax, 1
    mov bx, 2
    mov cx, 3
    mov dx, 4
    ;return to bootloader
    jmp 0:(STOP+BOOTLOADER_START)
