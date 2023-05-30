qemu-system-i386 -fda /opt/sandbox/pc_emu/freedos/144m/x86BOOT.img -S -s &
gdb -ex 'target remote localhost:1234' \
    -ex 'set disassembly-flavor intel' \
    -ex 'set architecture i8086' \
    -ex 'break *0x7c00' \
    -ex 'continue'
#qemu-system-i386 -fda /opt/projects/3rd/pcemu/8086tiny/fd.img -S -s -curses
#qemu-system-i386 -fda /opt/projects/3rd/pcemu/8086tiny/fd.img -curses
#qemu-system-i386 -fda /opt/sandbox/pc_emu/freedos/144m/x86BOOT.img -curses
