#ifndef ASM_HS_MMAP_H
#define ASM_HS_MMAP_H

void* do_mmap(int size);
void do_munmap(void* ptr, int size);

#endif // ASM_HS_MMAP_H
