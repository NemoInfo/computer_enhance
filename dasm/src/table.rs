use crate::exec::*;
use instruction_proc_macro::instructions;

instructions! {
  [MOV, exec_mov, [B(0b100010, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [MOV, exec_mov, [B(0b1100011, 7), W, MOD, B(0b000,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI, I(D,0)]],
  [MOV, exec_mov, [B(0b1011, 4), W, REG, DATA_LO, DATA_HI, I(D, 1)]],
  [MOV, exec_mov, [B(0b1010000, 7), W, ADDR_LO, ADDR_HI, I(REG, 0), I(D,1)]],
  [MOV, exec_mov, [B(0b1010001, 7), W, ADDR_LO, ADDR_HI, I(REG, 0), I(D,0)]],
  [MOV, exec_mov, [B(0b100011, 6), D, B(0,1), MOD, B(0,1), SR, R_M, DISP_LO, DISP_HI, I(W,1)]],

  [ADD, exec_add, [B(0b000000, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [ADD, exec_add, [B(0b100000, 6), S, W, MOD, B(0b000,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI_IF_SW, I(D,0)]],
  [ADD, exec_add, [B(0b0000010, 7), W, DATA_LO, DATA_HI, I(D,1), I(REG, 0)]],

  [SUB, exec_sub, [B(0b001010, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [SUB, exec_sub, [B(0b100000, 6), S, W, MOD, B(0b101,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI_IF_SW, I(D,0)]],
  [SUB, exec_sub, [B(0b0010110, 7), W, DATA_LO, DATA_HI, I(D,1), I(REG, 0)]],

  [CMP, exec_cmp, [B(0b001110, 6), D, W, MOD, REG, R_M, DISP_LO, DISP_HI]],
  [CMP, exec_cmp, [B(0b100000, 6), S, W, MOD, B(0b111,3), R_M, DISP_LO, DISP_HI, DATA_LO, DATA_HI_IF_SW, I(D,0)]],
  [CMP, exec_cmp, [B(0b0011110, 7), W, DATA_LO, DATA_HI, I(D,1), I(REG, 0)]],

  [JO , exec_todo, [B(0x70, 8), IP8, I(D,0)]],
  [JNO, exec_todo, [B(0x71, 8), IP8, I(D,0)]],
  [JB , exec_todo, [B(0x72, 8), IP8, I(D,0)]],
  [JNB, exec_todo, [B(0x73, 8), IP8, I(D,0)]],
  [JZ , exec_todo, [B(0x74, 8), IP8, I(D,0)]],
  [JNZ, exec_todo, [B(0x75, 8), IP8, I(D,0)]],
  [JNA, exec_todo, [B(0x76, 8), IP8, I(D,0)]],
  [JA , exec_todo, [B(0x77, 8), IP8, I(D,0)]],
  [JS , exec_todo, [B(0x78, 8), IP8, I(D,0)]],
  [JNS, exec_todo, [B(0x79, 8), IP8, I(D,0)]],
  [JP , exec_todo, [B(0x7a, 8), IP8, I(D,0)]],
  [JNP, exec_todo, [B(0x7b, 8), IP8, I(D,0)]],
  [JL , exec_todo, [B(0x7c, 8), IP8, I(D,0)]],
  [JNL, exec_todo, [B(0x7d, 8), IP8, I(D,0)]],
  [JNG, exec_todo, [B(0x7e, 8), IP8, I(D,0)]],
  [JG , exec_todo, [B(0x7f, 8), IP8, I(D,0)]],

  [LOOPNZ, exec_todo, [B(0xe0, 8), IP8, I(D,0)]],
  [LOOPZ , exec_todo, [B(0xe1, 8), IP8, I(D,0)]],
  [LOOP  , exec_todo, [B(0xe2, 8), IP8, I(D,0)]],
  [JCXZ  , exec_todo, [B(0xe3, 8), IP8, I(D,0)]],
}
