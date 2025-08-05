; listing_0039_more_movs disassembly
bits 16
mov si, bx                     ;0000
mov dh, al                     ;0002
mov cl, byte 12                ;0004
mov ch, byte -12               ;0006
mov cx, word 12                ;0008
mov cx, word -12               ;000b
mov dx, word 3948              ;000e
mov dx, word -3948             ;0011
mov al, [bx + si + 0]          ;0014
mov bx, [bp + di + 0]          ;0016
mov dx, [bp + 0]               ;0018
mov ah, [bx + si + 4]          ;001b
mov al, [bx + si + 4999]       ;001e
mov [bx + di + 0], cx          ;0022
mov [bp + si + 0], cl          ;0024
mov [bp + 0], ch               ;0026