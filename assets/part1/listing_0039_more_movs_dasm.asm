; listing_0039_more_movs disassembly
bits 16
mov si, bx
mov dh, al
mov cl, byte 12
mov ch, byte -12
mov cx, word 12
mov cx, word -12
mov dx, word 3948
mov dx, word -3948
mov al, [bx + si + 0]
mov bx, [bp + di + 0]
mov dx, [bp + 0]
mov ah, [bx + si + 4]
mov al, [bx + si + 4999]
mov [bx + di + 0], cx
mov [bp + si + 0], cl
mov [bp + 0], ch