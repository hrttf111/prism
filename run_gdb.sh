#gdb -ex 'target remote localhost:20001' \
#    -ex 'set disassembly-flavor intel' \
#    -ex 'set architecture i8086' \
#    -ex 'break *0x7c00' \
#    -ex 'continue'

gdb -ex 'target remote localhost:20001' \
    -ex 'set disassembly-flavor intel' \
    -ex 'layout asm' \
    -ex 'set architecture i8086'

# x/10c (16*$ds + $si + 10)
# 0x90a20 - execr
# 0xa08 - clock init
# 0x9a4b7 - ???
# 0x9ace4 - timer ends

#gdb -ex 'target remote localhost:20001' \
#    -ex 'set disassembly-flavor intel' \
#    -ex 'set architecture i8086' \
#    -ex 'layout asm' \
#    -ex 'break *0x9ace4' \
#    -ex 'cont'
