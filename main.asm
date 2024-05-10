; bytecode vm for 8086 msdos
;
; file `out` always gets cleared at start
;
; ## registers
; there are at least 256 byte sized registers.
; most operations (arithm, comp, ...) operate on pairs of registers (to form a word)
;
; ## format
; [opcode: 1B] [arg0: 1B] [arg1: 1B]
;
; ## operations
; | ';' C   | indicates end of file; required!; C = exit code |
; | 'E' C   | exits the program; C = exit code |
; | 'I' R V | immediate into reg (1B) R value V |
; | '@' R   | output ascii char in reg R (1B) |
; | '?' R   | reads ascii char into reg R (1B) |
; | 'G' A B | jump to addr immediate A (low), B (high) |
; | 'C' R D | jump to addr D (low), 0 (high) IF value in register (2B) R is zero |
; | 'A' D S | (2B) D = (2B) D + (2B) S |
; | 'S' D S | (2B) D = (2B) D - (2B) S |
; | 'L' D S | (2B) D = (2B) D << (2B) S |
; | 'R' D S | (2B) D = (2B) D >> (2B) S |
; | 'N' D S | (2B) D = ~((2B) D & (2B) S) |
; | 'M' D S | (1B) D = (1B) S |
; | 'Z' D S | (2B) D = (1B) S ; zero extend source (source is low, 0 is high) |
; | '>' S   | reads file `in` into the data buffer and stores the amount of read bytes into register (2B) S |
; | `<` R   | appends value in register (1B) R into file `out` |
; | '=' O I | reads value in data buffer at pos [ value in reg (2B) I ] into register (1B) O |

bits 16
  org 0x100

;==========
init_prog:
  ; create (overwrite) file
  mov cx, 0 ; todo: ?
  mov dx, outfile
  mov ah, 0x3C
  int 0x21
  mov dx, err_open
  jc error
  mov [prog_outf], ax
;==========

;==========
read_prog:
  ; open file
  mov al, 0
  mov dx, file
  mov ah, 0x3D
  int 0x21
  mov dx, err_open
  jc error
  ; file handle in ax

  ; read file
  mov bx, ax
  mov cx, 512
  mov dx, prog_buf
  mov ah, 0x3F
  int 0x21
  mov dx, err_read
  jc error
  ; number of bytes read in ax

  ; close file
  mov ah, 0x3E
  int 0x21
;==========

;==========
exec:
  mov si, word [prog_counter]

  mov al, [prog_buf + si] ; opcode
  mov ah, 0
  mov di, ax

  inc si
  mov bx, [prog_buf + si] ; (arg0, arg1)

  sub di, ';'
  add di, di
  mov di, [exec_lut + di]

  test di, di
  mov dx, err_instr
  jz short error

  jmp di
;==========

;==========
  mov al, 0
exit:
  ; close file
  mov bx, [prog_outf]
  mov ah, 0x3E
  int 0x21

  mov ah, 0x4C
  int 0x21
;==========

;==========
error: ; dx = error string $ terminated
  mov ah, 0x09
  int 0x21

  mov al, 1
  jmp exit
;==========

;==========
exec_op_exit: ; bl = exit code; bh =
  mov al, bl
  jmp exit
;==========

;==========
inc_pc:
  add word [prog_counter], 3
  jmp exec
;==========

;==========
exec_op_imm: ; bl = reg; bh = value
  mov al, bl
  mov ah, 0
  mov di, ax

  mov [prog_regs + di], bh
  jmp inc_pc
;==========

;==========
exec_op_out: ; bl = reg; bh =
  mov al, bl
  mov ah, 0
  mov si, ax

  mov dl, [prog_regs + si]
  mov ah, 0x02
  int 0x21
  jmp inc_pc
;==========

;==========
exec_op_in: ; bl = reg; bh =
  mov ah, 0x08
  int 0x21

  mov bh, 0
  mov di, bx

  mov [prog_regs + di], al

  jmp inc_pc
;==========

;==========
exec_op_goto: ; bx = pc
  mov [prog_counter], bx
  jmp exec
;==========

;==========
exec_op_cond: ; bl = test reg; bh = zp address
  mov al, bl
  mov ah, 0
  mov si, ax

  cmp word [prog_regs + si], 0
  jne inc_pc

  mov bl, bh
  mov bh, 0
  mov [prog_counter], bx
  jmp exec
;==========

;==========
dyadic_args:
  mov dl, bl
  mov dh, 0
  mov si, dx
  mov ax, [prog_regs + si]
  mov dl, bh
  mov si, dx
  mov cx, [prog_regs + si]
  ret
;==========

;==========
dyadic_post:
  mov bh, 0
  mov di, bx
  mov [prog_regs + di], ax

  jmp inc_pc
;==========

;==========
exec_op_add: ; bl = dest & src1; bh = src2
  call dyadic_args
  add ax, cx
  jmp dyadic_post
;==========

;==========
exec_op_sub: ; bl = dest & src1; bh = src2
  call dyadic_args
  sub ax, cx
  jmp dyadic_post
;==========

;==========
exec_op_lshift: ; bl = dest & src1; bh = src2
  call dyadic_args
  shl ax, cl
  jmp dyadic_post
;==========

;==========
exec_op_rshift: ; bl = dest & src1; bh = src2
  call dyadic_args
  shr ax, cl
  jmp dyadic_post
;==========

;==========
exec_op_bwnand: ; bl = dest & src1; bh = src2
  call dyadic_args
  and ax, cx
  not ax
  jmp dyadic_post
;==========

;==========
exec_op_mov: ; bl = dest; bh = src
  mov al, bh
  mov ah, 0
  mov si, ax
  mov al, [prog_regs + si]
  mov bh, 0
  mov di, bx
  mov [prog_regs + di], al

  jmp inc_pc
;==========

;==========
exec_op_zext: ; bl = dest; bh = src
  mov cl, bh
  mov ch, 0
  mov si, cx

  mov al, [prog_regs + si]
  mov ah, 0
  mov bh, 0
  mov di, bx
  mov [prog_regs + di], ax

  jmp inc_pc
;==========

;==========
exec_op_append: ; bl = char reg
  mov bh, 0
  mov si, bx
  mov al, [prog_regs + si]
  mov [buf_byte], al

  mov ah, 0x40
  mov bx, [prog_outf]
  mov cx, 1
  mov dx, buf_byte
  int 0x21
  jc .error

  jmp inc_pc

.error:
  jmp error
;==========

;==========
exec_op_read: ; bl = amount of bytes out reg
  mov bh, 0
  mov di, bx

  ; open file
  mov al, 0
  mov dx, infile
  mov ah, 0x3D
  int 0x21
  mov dx, err_open
  jc .error

  ; read file
  mov bx, ax
  mov cx, prog_readr_len
  mov dx, prog_readr
  mov ah, 0x3F
  int 0x21
  mov dx, err_read
  jc .error

  ; save how many read
  mov [prog_regs + di], ax

  ; close file
  mov ah, 0x3E
  int 0x21

  jmp inc_pc

.error:
  jmp error
;==========

;==========
exec_op_index: ; bl = out reg; bh = index reg
  mov al, bh
  mov ah, 0
  mov si, ax
  mov si, [prog_regs + si]

  mov al, [prog_readr + si]

  mov bh, 0
  mov di, bx
  mov [prog_regs + di], al

  jmp inc_pc
;==========

;=======================================
;==                DATA               ==
;=======================================

; for debugging purposes (in disassembly view)
dw 0xDADA, 0

;==========
file:
  db "test.bin", 0
outfile:
  db "out", 0
infile:
  db "in", 0
;==========

;==========
err_open:
  db "could not open file", '$'
err_read:
  db "could not read file", '$'
err_instr:
  db "instr not found", '$'
;==========

;==========
exec_lut: ; usage: index opcode - ';'
  dw exec_op_exit   ; ';'
  dw exec_op_append ; '<'
  dw exec_op_index  ; '='
  dw exec_op_read   ; '>'
  dw exec_op_in     ; '?'
  dw exec_op_out    ; '@'
  dw exec_op_add    ; 'A'
  dw 0              ; 'B'
  dw exec_op_cond   ; 'C'
  dw 0              ; 'D'
  dw exec_op_exit   ; 'E'
  dw 0              ; 'F'
  dw exec_op_goto   ; 'G'
  dw 0              ; 'H'
  dw exec_op_imm    ; 'I'
  dw 0              ; 'J'
  dw 0              ; 'K'
  dw exec_op_lshift ; 'L'
  dw exec_op_mov    ; 'M'
  dw exec_op_bwnand ; 'N'
  dw 0              ; 'O'
  dw 0              ; 'P'
  dw 0              ; 'Q'
  dw exec_op_rshift ; 'R'
  dw exec_op_sub    ; 'S'
  dw 0              ; 'T'
  dw 0              ; 'U'
  dw 0              ; 'V'
  dw 0              ; 'W'
  dw 0              ; 'X'
  dw 0              ; 'Y'
  dw exec_op_zext   ; 'Z'
;==========

;==========
buf_byte:
  db 0
;==========

;==========
prog_counter:
  dw 0
prog_regs:
  times 256 db 0
prog_outf:
  dw 0
prog_buf: ; can be extended to any word indexable len
  times 512 db 0
prog_readr: ; can be extended to any word indexable len
  times 512 db 0
prog_readr_len: equ $-prog_readr
;==========
