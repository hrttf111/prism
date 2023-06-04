#gdb -ex 'target remote localhost:20001' \
#    -ex 'set disassembly-flavor intel' \
#    -ex 'set architecture i8086' \
#    -ex 'break *0x7c00' \
#    -ex 'continue'

#gdb -ex 'target remote localhost:20001' \
#    -ex 'set disassembly-flavor intel' \
#    -ex 'set architecture i8086'

gdb -ex 'target remote localhost:20001' \
    -ex 'set disassembly-flavor intel' \
    -ex 'set architecture i8086' \
    -ex 'layout asm' \
    -ex 'break *0x27b26' \
    -ex 'cont'
    #-ex 'break *0x27b14'
    #-ex 'break *0x27af3'
    #-ex 'break *0x27ac5'
