#gdb -ex 'target remote localhost:20001' \
#    -ex 'set disassembly-flavor intel' \
#    -ex 'set architecture i8086' \
#    -ex 'break *0x7c00' \
#    -ex 'continue'

#gdb -ex 'target remote localhost:20001' \
#    -ex 'set disassembly-flavor intel' \
#    -ex 'set architecture i8086'

# 0x90a20
# x/10c (16*$ds + $si + 10)

gdb -ex 'target remote localhost:20001' \
    -ex 'set disassembly-flavor intel' \
    -ex 'set architecture i8086' \
    -ex 'layout asm' \
    -ex 'break *0x90a20' \
    -ex 'cont'
    #-ex 'break *0x90a15' \
    #-ex 'break *162770' \
    #-ex 'break *2330' \
    #-ex 'break *592427' \
    #-ex 'break *592438' \
    #-ex 'break *216' \
    #-ex 'break *0x91d8b' \
    #-ex 'break *0x9c806' \
    #-ex 'break *0x9c913' \
    #-ex 'break *0x9d845' \
    #-ex 'break *0x99ab5' \
    #-ex 'break *0x0000cb40' \
    #-ex 'break *0x12d79' \
    #-ex 'break *0x12d1a' \
    #-ex 'break *0x12cf9' \
    #-ex 'break *0x632' \
    #-ex 'break *0x27b26' \
    #-ex 'break *0x27b14'
    #-ex 'break *0x27af3'
    #-ex 'break *0x27ac5'
