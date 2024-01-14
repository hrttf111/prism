# Prism
Prism is a emulator of an original Intel 8086 CPU. It has limited support of PC XT hardware and a custom BIOS emulator.

## Capabilities
 - Emulation of Intel 8086 instructions
 - Supported BIOS interrupts:
 - Video text mode (MDA)
 - Keyboard
 - Floppy (1.44)
 - PIC (8259)
 - PIT (8253)

## Debug
Prism has multiple mechanisms for debugging.

GDB
There is a custom GDB server implementation which allows to:
 - read/write registers
 - read/write memory
 - set breakpoints
 - execute next instruction (ni)
 - continue execution (c)

Logging
Prism provides logging which allows to trace its internal state.

## Limitations
There are many limitations:
 - Only floppy storage device (1.44) is supported
 - Only PIT and PIC HW devices were implemented according to specification
 - No math co-processor support (8087)
 - Displaying data via direct writes to video memory is not supported
 - No sound
 - No graphics mode
 - Only x86 is supported as for the main application
 - Step interrupt is not supported

## Testing
Prism is covered by multiple layers of tests: basic infra tests, tests for GDB server, end-to-end CPU/peripheral test.
The most extensive are end-to-end tests (test-asm). They allow to test CPU accuracy in different cases, they cover all CPU instructions except: wait, lock and into. They also cover peripheral's infrastructure and BIOS.
End-to-end tests consist of portion of valid 16-bit x86 code written in Intel assembly (NASM syntax) and checks for the results. Simultaneous exection by multiple emulators is supported, it allows to run same code in Prism and other emulators and compare results. Currently 2 external emulators are supported:
 - QEMU
 - Native amd64/x86_64 (not an emulator, works only when Prism runs on amd64 CPU)
