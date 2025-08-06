; listing_0043_immediate_movs disassembly
bits 16
mov ax, word 1       ; [0000]  (ax):0000->0001
mov bx, word 2       ; [0003]  (bx):0000->0002
mov cx, word 3       ; [0006]  (cx):0000->0003
mov dx, word 4       ; [0009]  (dx):0000->0004
mov sp, word 5       ; [000c]  (sp):0000->0005
mov bp, word 6       ; [000f]  (bp):0000->0006
mov si, word 7       ; [0012]  (si):0000->0007
mov di, word 8       ; [0015]  (di):0000->0008

; Register State:
;      A 0001
;      C 0003
;      D 0004
;      B 0002
;     SP 0005
;     BP 0006
;     SI 0007
;     DI 0008
;     ES 0000
;     CS 0000
;     SS 0000
;     DS 0000
;     IP 0018
;  FLAGS 0000