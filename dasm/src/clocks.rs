use std::{
  fmt::{Debug, Display},
  ops::{Add, AddAssign},
};

use crate::{types::Instruction, EffectiveAddressExpression, InstructionFlag};

#[derive(Debug, Clone, Copy)]
pub struct InstructionTimingInterval {
  min: u32,
  max: u32,
}

impl Add<u32> for InstructionTimingInterval {
  type Output = Self;
  fn add(self, rhs: u32) -> Self::Output {
    Self { min: self.min + rhs, max: self.max + rhs }
  }
}

impl Add<InstructionTimingInterval> for InstructionTimingInterval {
  type Output = Self;
  fn add(self, rhs: InstructionTimingInterval) -> Self::Output {
    Self { min: self.min + rhs.min, max: self.max + rhs.max }
  }
}

impl AddAssign<InstructionTimingInterval> for InstructionTimingInterval {
  fn add_assign(&mut self, rhs: InstructionTimingInterval) {
    *self = *self + rhs
  }
}

impl Display for InstructionTimingInterval {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if self.min == self.max {
      write!(f, "{}", self.min)
    } else {
      write!(f, "[{}, {}]", self.min, self.max)
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Processor {
  I8088,
  I8086,
}

impl TryFrom<&str> for Processor {
  type Error = ();
  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "8086" => Ok(Self::I8086),
      "8088" => Ok(Self::I8088),
      _ => Err(()),
    }
  }
}

#[derive(Clone, Copy)]
pub struct InstructionTiming {
  base: InstructionTimingInterval,
  transfers: u32,
  ea: u32,
}

impl InstructionTiming {
  pub fn to_debug_string(&self, mut address_unaligned: bool, processor: Processor) -> String {
    if let Processor::I8088 = processor {
      address_unaligned = true
    }
    format!(
      "{:<2} [{:>2} + {}ea {:<2}]",
      self.clocks(address_unaligned, processor).to_string(),
      self.base.to_string(),
      if address_unaligned { format!("tf {} * 4 + ", self.transfers) } else { "".into() },
      self.ea.to_string()
    )
  }

  pub fn to_debug_string_no_address(&self, processor: Processor) -> String {
    if let Processor::I8088 = processor {
      return self.to_debug_string(true, processor);
    }
    format!(
      "{:<2} [{:>2} + {}ea {:<2}]",
      format!("+[{:<2} | {:>2}]", self.clocks(false, processor).to_string(), self.clocks(true, processor).to_string()),
      self.base.to_string(),
      format!("(tf {} * 4) + ", self.transfers),
      self.ea.to_string()
    )
  }

  // For the 8086 +4 clocks for each 16-bit word transfer with an odd address.
  // For the 8088 +4 clocks for each 16-bit word transfer
  pub fn clocks(&self, addres_unaligned: bool, processor: Processor) -> InstructionTimingInterval {
    match processor {
      Processor::I8086 => self.base + self.transfers * 4 * addres_unaligned as u32 + self.ea,
      Processor::I8088 => self.base + self.transfers * 4 + self.ea,
    }
  }
}

impl EffectiveAddressExpression {
  fn clocks(&self, inst: &Instruction) -> u32 {
    use crate::types::reg::*;
    use crate::types::EffectiveAddressTerm as Term;
    let mut clocks = match self {
      EffectiveAddressExpression::Direct(_) => 6,
      EffectiveAddressExpression::Expression { terms, displacement } => {
        displacement.signum().abs() as u32 * 4
          + match terms {
            [Some(_), None] => 5,
            [Some(Term { register: BP }), Some(Term { register: DI })]
            | [Some(Term { register: BX }), Some(Term { register: SI })] => 7,
            [Some(Term { register: BP }), Some(Term { register: SI })]
            | [Some(Term { register: BX }), Some(Term { register: DI })] => 8,
            _ => panic!("Invalid EffectiveAddressExpression {self:?}"),
          }
      }
    };

    if inst.flags & InstructionFlag::SegmentOverride as u16 != 0 {
      clocks += 2;
    }

    clocks
  }
}

impl InstructionTiming {
  fn new_fixed(clocks: u32, transfers: u32, ea: u32) -> Self {
    InstructionTiming { base: InstructionTimingInterval { min: clocks, max: clocks }, transfers, ea }
  }

  fn _new_interval(min: u32, max: u32, transfers: u32, ea: u32) -> Self {
    InstructionTiming { base: InstructionTimingInterval { min, max }, transfers, ea }
  }
}

pub struct TimingState {
  pub current_time: InstructionTimingInterval,
  pub branch_taken: bool,
}

impl TimingState {
  pub fn new() -> Self {
    Self { branch_taken: false, current_time: InstructionTimingInterval { min: 0, max: 0 } }
  }
}

impl TimingState {
  pub fn estimate_instruction_clocks(&self, inst: &Instruction) -> InstructionTiming {
    use crate::table::InstructionKind::*;
    use crate::types::reg::*;
    use crate::types::Operand::*;

    match inst.kind {
      MOV => match inst.operands {
        [Memory(_), Register(AX | AL | AH)] => InstructionTiming::new_fixed(10, 1, 0),
        [Register(AX | AL | AH), Memory(_)] => InstructionTiming::new_fixed(10, 1, 0),
        [Register(DS | CS | SS | ES), Register(_)] => InstructionTiming::new_fixed(2, 0, 0),
        [Register(DS | CS | SS | ES), Memory(ea)] => InstructionTiming::new_fixed(8, 0, ea.clocks(inst)),
        [Register(_), Register(DS | CS | SS | ES)] => InstructionTiming::new_fixed(2, 0, 0),
        [Memory(ea), Register(DS | CS | SS | ES)] => InstructionTiming::new_fixed(9, 0, ea.clocks(inst)),
        [Register(_), Register(_)] => InstructionTiming::new_fixed(2, 0, 0),
        [Register(_), Memory(ea)] => InstructionTiming::new_fixed(8, 1, ea.clocks(inst)),
        [Memory(ea), Register(_)] => InstructionTiming::new_fixed(9, 1, ea.clocks(inst)),
        [Register(_), Immediate(_)] => InstructionTiming::new_fixed(4, 0, 0),
        [Memory(ea), Immediate(_)] => InstructionTiming::new_fixed(10, 1, ea.clocks(inst)),
        _ => panic!("Invalid arithmetic operands combination {:?}", inst.operands),
      },
      ADD | SUB => match inst.operands {
        [Register(_), Register(_)] => InstructionTiming::new_fixed(3, 0, 0),
        [Register(_), Memory(ea)] => InstructionTiming::new_fixed(9, 1, ea.clocks(inst)),
        [Memory(ea), Register(_)] => InstructionTiming::new_fixed(16, 2, ea.clocks(inst)),
        [Register(_), Immediate(_)] => InstructionTiming::new_fixed(4, 0, 0),
        [Memory(ea), Immediate(_)] => InstructionTiming::new_fixed(17, 2, ea.clocks(inst)),
        _ => panic!("Invalid arithmetic operands combination {:?}", inst.operands),
      },
      CMP => match inst.operands {
        [Register(_), Register(_)] => InstructionTiming::new_fixed(3, 0, 0),
        [Register(_), Memory(ea)] => InstructionTiming::new_fixed(9, 1, ea.clocks(inst)),
        [Memory(ea), Register(_)] => InstructionTiming::new_fixed(9, 1, ea.clocks(inst)),
        [Register(_), Immediate(_)] => InstructionTiming::new_fixed(4, 0, 0),
        [Memory(ea), Immediate(_)] => InstructionTiming::new_fixed(10, 1, ea.clocks(inst)),
        _ => panic!("Invalid arithmetic operands combination {:?}", inst.operands),
      },
      JO | JNO | JB | JNB | JZ | JNZ | JNA | JA | JS | JNS | JP | JNP | JL | JNL | JNG | JG => {
        InstructionTiming::new_fixed(if self.branch_taken { 16 } else { 4 }, 0, 0)
      }
      LOOPNZ => InstructionTiming::new_fixed(if self.branch_taken { 19 } else { 5 }, 0, 0),
      LOOPZ | JCXZ => InstructionTiming::new_fixed(if self.branch_taken { 18 } else { 6 }, 0, 0),
      LOOP => InstructionTiming::new_fixed(if self.branch_taken { 17 } else { 5 }, 0, 0),
      SEGMENT => InstructionTiming::new_fixed(2, 0, 0),
      // other => todo!("Timing not implemeted for {other:?}"),
    }
  }
}
