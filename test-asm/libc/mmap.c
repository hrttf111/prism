#include "mmap.h"

#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#define _GNU_SOURCE
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

void* do_mmap(int size)
{
    void* mem_ptr = mmap(0, size, PROT_WRITE|PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    if (mem_ptr == MAP_FAILED) {
        return NULL;
    }
    return mem_ptr;
}

void do_munmap(void* ptr, int size)
{
    munmap(ptr, size);
}
