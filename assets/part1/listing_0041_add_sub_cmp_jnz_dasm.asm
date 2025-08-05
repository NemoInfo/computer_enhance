; listing_0041_add_sub_cmp_jnz disassembly
bits 16
add bx, [bx + si + 0]          ;0000
add bx, [bp + 0]               ;0002
add si, word 2                 ;0005
add bp, word 2                 ;0008
add cx, word 8                 ;000b
add bx, [bp + 0]               ;000e
add cx, [bx + 2]               ;0011
add bh, [bp + si + 4]          ;0014
add di, [bp + di + 6]          ;0017
add [bx + si + 0], bx          ;001a
add [bp + 0], bx               ;001c
add [bp + 0], bx               ;001f
add [bx + 2], cx               ;0022
add [bp + si + 4], bh          ;0025
add [bp + di + 6], di          ;0028
add [bx + 0], byte 34          ;002b
add [bp + si + 1000], word 29  ;002e
add ax, [bp + 0]               ;0033
add al, [bx + si + 0]          ;0036
add ax, bx                     ;0038
add al, ah                     ;003a
add ax, word 1000              ;003c
add al, byte -30               ;003f
add al, byte 9                 ;0041
sub bx, [bx + si + 0]          ;0043
sub bx, [bp + 0]               ;0045
sub si, word 2                 ;0048
sub bp, word 2                 ;004b
sub cx, word 8                 ;004e
sub bx, [bp + 0]               ;0051
sub cx, [bx + 2]               ;0054
sub bh, [bp + si + 4]          ;0057
sub di, [bp + di + 6]          ;005a
sub [bx + si + 0], bx          ;005d
sub [bp + 0], bx               ;005f
sub [bp + 0], bx               ;0062
sub [bx + 2], cx               ;0065
sub [bp + si + 4], bh          ;0068
sub [bp + di + 6], di          ;006b
sub [bx + 0], byte 34          ;006e
sub [bx + di + 0], word 29     ;0071
sub ax, [bp + 0]               ;0074
sub al, [bx + si + 0]          ;0077
sub ax, bx                     ;0079
sub al, ah                     ;007b
sub ax, word 1000              ;007d
sub al, byte -30               ;0080
sub al, byte 9                 ;0082
cmp bx, [bx + si + 0]          ;0084
cmp bx, [bp + 0]               ;0086
cmp si, word 2                 ;0089
cmp bp, word 2                 ;008c
cmp cx, word 8                 ;008f
cmp bx, [bp + 0]               ;0092
cmp cx, [bx + 2]               ;0095
cmp bh, [bp + si + 4]          ;0098
cmp di, [bp + di + 6]          ;009b
cmp [bx + si + 0], bx          ;009e
cmp [bp + 0], bx               ;00a0
cmp [bp + 0], bx               ;00a3
cmp [bx + 2], cx               ;00a6
cmp [bp + si + 4], bh          ;00a9
cmp [bp + di + 6], di          ;00ac
cmp [bx + 0], byte 34          ;00af
cmp [4834], word 29            ;00b2
cmp ax, [bp + 0]               ;00b7
cmp al, [bx + si + 0]          ;00ba
cmp ax, bx                     ;00bc
cmp al, ah                     ;00be
cmp ax, word 1000              ;00c0
cmp al, byte -30               ;00c3
cmp al, byte 9                 ;00c5
jnz $+4                        ;00c7
jnz $-2                        ;00c9
jnz $-4                        ;00cb
jnz $-2                        ;00cd
jz $+0                         ;00cf
jl $-2                         ;00d1
jng $-4                        ;00d3
jb $-6                         ;00d5
jna $-8                        ;00d7
jp $-10                        ;00d9
jo $-12                        ;00db
js $-14                        ;00dd
jnz $-16                       ;00df
jnl $-18                       ;00e1
jg $-20                        ;00e3
jnb $-22                       ;00e5
ja $-24                        ;00e7
jnp $-26                       ;00e9
jno $-28                       ;00eb
jns $-30                       ;00ed
loop $-32                      ;00ef
loopz $-34                     ;00f1
loopnz $-36                    ;00f3
jcxz $-38                      ;00f5