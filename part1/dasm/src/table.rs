use crate::exec::*;
use instruction_proc_macro::instructions;

instructions! {
  [MOV, exec_mov, [B(0b100010, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [MOV, exec_mov, [B(0b1100011, 7), W, MOD, B(0b000,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI, I(D,0)]],
  [MOV, exec_mov, [B(0b1011, 4), W, REG, DATA_LO, DATA_HI, I(D, 1)]],
  [MOV, exec_mov, [B(0b1010000, 7), W, ADDR_LO, ADDR_HI, I(REG, 0), I(D,1)]],
  [MOV, exec_mov, [B(0b1010001, 7), W, ADDR_LO, ADDR_HI, I(REG, 0), I(D,0)]],
  [MOV, exec_mov, [B(0b100011, 6), D, B(0,1), MOD, B(0,1), SR, R_M, DISP_LO, DISP_HI, I(W,1)]],

  [PUSH, exec_push, [B(0xFF, 8), MOD, B(0b110, 3), R_M, DISP_LO, DISP_HI, I(W, 1), I(D, 1)]],
  [PUSH, exec_push, [B(0b01010, 5), REG, I(W, 1)]],
  [PUSH, exec_push, [B(0b000, 3), SR, B(0b110, 3), I(W, 1), I(D, 0)]],

  [POP, exec_pop, [B(0b10001111, 8), MOD, B(0b000, 3), R_M, DISP_LO, DISP_HI, I(W, 1), I(D, 0)]],
  [POP, exec_pop, [B(0b01011, 5), REG, I(W, 1), I(D, 1)]],
  [POP, exec_pop, [B(0b000, 3), SR, B(0b111, 3), I(W, 1), I(D, 1)]],

  [EXCHG, exec_exchg, [B(0b1000011, 7), W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [EXCHG, exec_exchg, [B(0b10010, 5), I(MOD, 0b11), REG, I(W, 1), I(R_M, 0)]],

  [IN, err_exec_io, [B(0b1110010, 7), W, DATA8, I(REG, 0), I(D, 1)]],
  [IN, err_exec_io, [B(0b1110110, 7), W, I(REG, 0), I(R_M, 0b010), I(MOD, 0b11), I(D, 1), I(W_R_M, 1)]],

  [OUT, err_exec_io, [B(0b1110011, 7), W, DATA8, I(REG, 0), I(D, 0)]],
  [OUT, err_exec_io, [B(0b1110111, 7), W, I(REG, 0), I(R_M, 0b010), I(MOD, 0b11), I(D, 0), I(W_R_M, 1)]],

  [XLAT, exec_xlat, [B(0b11010111, 8)]],
  [LEA , exec_lea , [B(0b10001101, 8), MOD, REG, R_M, DISP_LO, DISP_HI, I(W, 1), I(D, 1)]],

  [ADD, exec_add, [B(0b000000, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [ADD, exec_add, [B(0b100000, 6), S, W, MOD, B(0b000,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI_IF_SW, I(D,0)]],
  [ADD, exec_add, [B(0b0000010, 7), W, DATA_LO, DATA_HI, I(D,1), I(REG, 0)]],

  [SUB, exec_sub, [B(0b001010, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [SUB, exec_sub, [B(0b100000, 6), S, W, MOD, B(0b101,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI_IF_SW, I(D,0)]],
  [SUB, exec_sub, [B(0b0010110, 7), W, DATA_LO, DATA_HI, I(D,1), I(REG, 0)]],

  [CMP, exec_cmp, [B(0b001110, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [CMP, exec_cmp, [B(0b100000, 6), S, W, MOD, B(0b111,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI_IF_SW, I(D,0)]],
  [CMP, exec_cmp, [B(0b0011110, 7), W, DATA_LO, DATA_HI, I(D,1), I(REG, 0)]],

  [JO , exec_jo , [B(0x70, 8), IP8]],
  [JNO, exec_jno, [B(0x71, 8), IP8]],
  [JB , exec_jb , [B(0x72, 8), IP8]],
  [JNB, exec_jnb, [B(0x73, 8), IP8]],
  [JZ , exec_jz , [B(0x74, 8), IP8]],
  [JNZ, exec_jnz, [B(0x75, 8), IP8]],
  [JNA, exec_jna, [B(0x76, 8), IP8]],
  [JA , exec_ja , [B(0x77, 8), IP8]],
  [JS , exec_js , [B(0x78, 8), IP8]],
  [JNS, exec_jns, [B(0x79, 8), IP8]],
  [JP , exec_jp , [B(0x7a, 8), IP8]],
  [JNP, exec_jnp, [B(0x7b, 8), IP8]],
  [JL , exec_jl , [B(0x7c, 8), IP8]],
  [JNL, exec_jnl, [B(0x7d, 8), IP8]],
  [JNG, exec_jng, [B(0x7e, 8), IP8]],
  [JG , exec_jg , [B(0x7f, 8), IP8]],

  [LOOPNZ, exec_loopnz, [B(0xe0, 8), IP8]],
  [LOOPZ , exec_loopz , [B(0xe1, 8), IP8]],
  [LOOP  , exec_loop  , [B(0xe2, 8), IP8]],
  [JCXZ  , exec_jcxz  , [B(0xe3, 8), IP8]],

  [SEGMENT, exec_continue, [B(0b001, 3), SR, B(0b110, 3), I(D,0)]],
}
