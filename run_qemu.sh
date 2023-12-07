#qemu-system-i386 -fda /opt/sandbox/pc_emu/freedos/144m/x86BOOT.img -m 1M -object memory-backend-file,id=pc.ram,size=1M,mem-path=mem.data,prealloc=on,share=on -machine memory-backend=pc.ram
#qemu-system-i386 -fda ./data/bios_test -m 1M -object memory-backend-file,id=pc.ram,size=1M,mem-path=mem.data,prealloc=on,share=on -machine memory-backend=pc.ram
#qemu-system-i386 -device loader,addr=0x7c00,cpu-num=0,file=data/bios_test -m 1M -object memory-backend-file,id=pc.ram,size=1M,mem-path=mem.data,prealloc=on,share=on -machine memory-backend=pc.ram
qemu-system-i386 -device loader,addr=0,cpu-num=0,file=data/bios_test -m 1M -object memory-backend-file,id=pc.ram,size=1M,mem-path=mem.data,prealloc=on,share=on -machine memory-backend=pc.ram
#qemu-system-i386 -fda /opt/sandbox/pc_emu/freedos/144m/x86BOOT.img --mem-path ./mem.data -m 1M
#qemu-system-i386 -fda /opt/sandbox/pc_emu/freedos/144m/x86BOOT.img -S -s &
#gdb -ex 'target remote localhost:1234' -ex 'set disassembly-flavor intel' -ex 'set architecture i8086' -ex 'break *0x27b26'
#gdb -ex 'target remote localhost:1234' \
#    -ex 'set disassembly-flavor intel' \
#    -ex 'set architecture i8086' \
#    -ex 'break *0x27b26'
    #-ex 'break *0x7c55' \
    #-ex 'break *0x7c00' \
    #-ex 'continue'
#qemu-system-i386 -fda /opt/projects/3rd/pcemu/8086tiny/fd.img -S -s -curses
#qemu-system-i386 -fda /opt/projects/3rd/pcemu/8086tiny/fd.img -curses
#qemu-system-i386 -fda /opt/sandbox/pc_emu/freedos/144m/x86BOOT.img -curses
