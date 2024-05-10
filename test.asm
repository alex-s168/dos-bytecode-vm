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

Size:  equ 0
Char:  equ 2
One:   equ 3
ZeroB: equ 4

db 'I', One, 1     ; 0
db 'I', ZeroB, 0   ; 3


db '>', Size, 0    ; 6


db 'C', Size, 24   ; 9

db '=', Char, Size ; 12
db '@', Char, 0    ; 15

db 'S', Size, One  ; 18

db 'G', 9, 0       ; 21

db ';', 0, 0       ; 24
