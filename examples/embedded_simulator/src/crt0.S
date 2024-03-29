        /**********/
        /* _start */
        /**********/

        .section .start
        .globl _start
        .type _start,@function

_start:
.option push
.option norelax
        la gp, __global_pointer$
.option pop
        la sp, __stack_end


        /* Clear bss section */
        .type _startup_clear_bss,@function
_startup_clear_bss:
        la a0, __bss_start
        la a1, __bss_end
        bgeu a0, a1, 2f
1:
        sw zero, (a0)
        addi a0, a0, 4
        bltu a0, a1, 1b
2:
        .size _startup_clear_bss, . - _startup_clear_bss

        call main
        call __gnat_exit
2:      j 2b

        .globl __gnat_exit
        .type __gnat_exit,@function
        .globl _abort
        .type abort,@function
abort:
__gnat_exit:

        /* Use ebreak on RISC-V32 until QEMU is updated to 4.0 */
        ebreak

        j __gnat_exit

        /* Weak alias _exit to __gnat_exit */
        .weak      _exit
        .set _exit,__gnat_exit