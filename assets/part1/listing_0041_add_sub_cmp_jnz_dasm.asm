; listing_0041_add_sub_cmp_jnz disassembly
bits 16
add bx, [bx + si + 0]
add bx, [bp + 0]
add si, word 2
add bp, word 2
add cx, word 8
add bx, [bp + 0]
add cx, [bx + 2]
add bh, [bp + si + 4]
add di, [bp + di + 6]
add [bx + si + 0], bx
add [bp + 0], bx
add [bp + 0], bx
add [bx + 2], cx
add [bp + si + 4], bh
add [bp + di + 6], di
add [bx + 0], byte 34
add [bp + si + 1000], word 29
add ax, [bp + 0]
add al, [bx + si + 0]
add ax, bx
add al, ah
add ax, word 1000
add al, byte -30
add al, byte 9
sub bx, [bx + si + 0]
sub bx, [bp + 0]
sub si, word 2
sub bp, word 2
sub cx, word 8
sub bx, [bp + 0]
sub cx, [bx + 2]
sub bh, [bp + si + 4]
sub di, [bp + di + 6]
sub [bx + si + 0], bx
sub [bp + 0], bx
sub [bp + 0], bx
sub [bx + 2], cx
sub [bp + si + 4], bh
sub [bp + di + 6], di
sub [bx + 0], byte 34
sub [bx + di + 0], word 29
sub ax, [bp + 0]
sub al, [bx + si + 0]
sub ax, bx
sub al, ah
sub ax, word 1000
sub al, byte -30
sub al, byte 9
cmp bx, [bx + si + 0]
cmp bx, [bp + 0]
cmp si, word 2
cmp bp, word 2
cmp cx, word 8
cmp bx, [bp + 0]
cmp cx, [bx + 2]
cmp bh, [bp + si + 4]
cmp di, [bp + di + 6]
cmp [bx + si + 0], bx
cmp [bp + 0], bx
cmp [bp + 0], bx
cmp [bx + 2], cx
cmp [bp + si + 4], bh
cmp [bp + di + 6], di
cmp [bx + 0], byte 34
cmp [4834], word 29
cmp ax, [bp + 0]
cmp al, [bx + si + 0]
cmp ax, bx
cmp al, ah
cmp ax, word 1000
cmp al, byte -30
cmp al, byte 9
jnz $+4
jnz $-2
jnz $-4
jnz $-2
jz $+0
jl $-2
jng $-4
jb $-6
jna $-8
jp $-10
jo $-12
js $-14
jnz $-16
jnl $-18
jg $-20
jnb $-22
ja $-24
jnp $-26
jno $-28
jns $-30
loop $-32
loopz $-34
loopnz $-36
jcxz $-38