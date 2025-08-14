#![allow(non_snake_case)]
#![allow(non_camel_case_types)]

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use std::{collections::HashSet, str::FromStr};
use strum::EnumString;
use syn::{
  parenthesized,
  parse::{Parse, ParseStream, Parser},
  parse_macro_input, DeriveInput, Expr, ExprLit, Ident, Lit, Result, Token,
};

#[derive(Debug, Clone, Copy, std::cmp::Eq, std::cmp::PartialEq, Hash, EnumString)]
enum BitArg {
  REG,
  MOD,
  R_M,
  D,
  S,
  SR,
  W,
  W_R_M,
  IP8,
  DISP_LO,
  DISP_HI,
  DATA_LO,
  DATA8,
  DATA_HI,
  DATA_HI_IF_SW,
  ADDR_LO,
  ADDR_HI,
}

impl BitArg {
  fn to_ident(&self) -> Ident {
    let s = format!("{self:?}");
    format_ident!("{}_", s.to_lowercase())
  }
}

#[derive(Debug, Clone, Copy, std::cmp::Eq, std::cmp::PartialEq, Hash)]
enum BitVal {
  Lit { pattern: u8, len: u8 },
  Arg(BitArg),
  Imp { arg: BitArg, value: u8 },
}

impl BitVal {
  const fn len(&self) -> u8 {
    match self {
      BitVal::Lit { len, .. } => *len,
      BitVal::Imp { .. } => 0,
      BitVal::Arg(arg) => match arg {
        BitArg::REG => 3,
        BitArg::MOD => 2,
        BitArg::R_M => 3,
        BitArg::S => 1,
        BitArg::SR => 2,
        BitArg::D => 1,
        BitArg::W => 1,
        BitArg::W_R_M => 1,
        BitArg::IP8 => 8,
        BitArg::DISP_LO => 8,
        BitArg::DISP_HI => 8,
        BitArg::DATA8 => 8,
        BitArg::DATA_LO => 8,
        BitArg::DATA_HI => 8,
        BitArg::DATA_HI_IF_SW => 8,
        BitArg::ADDR_LO => 8,
        BitArg::ADDR_HI => 8,
      },
    }
  }
}

fn extract_u8_literal(expr: Expr) -> Result<u8> {
  match expr {
    Expr::Lit(ExprLit { lit: Lit::Int(int), .. }) => {
      int.base10_parse::<u8>().map_err(|e| syn::Error::new(int.span(), e))
    }
    _ => Err(syn::Error::new_spanned(expr, "Expected integer literal")),
  }
}

impl Parse for BitVal {
  fn parse(input: ParseStream) -> Result<Self> {
    let lookahead = input.lookahead1();

    if lookahead.peek(Ident) {
      let ident: Ident = input.parse()?;
      match ident.to_string().as_str() {
        "B" => {
          let content;
          parenthesized!(content in input);

          let pattern_expr: Expr = content.parse()?;
          content.parse::<Token![,]>()?;
          let len_expr: Expr = content.parse()?;

          // Try to extract the u8 values from the expressions
          let pattern = extract_u8_literal(pattern_expr)?;
          let len = extract_u8_literal(len_expr)?;

          assert!((pattern as u16) < 1u16 << (len as u16), "Bit literal out of bounds");

          Ok(BitVal::Lit { pattern, len })
        }
        "I" => {
          let content;
          parenthesized!(content in input);

          let arg: BitVal = content.parse()?;
          content.parse::<Token![,]>()?;
          let len_expr: Expr = content.parse()?;

          let BitVal::Arg(arg) = arg else {
            panic!("Can only pass implied args");
          };

          let value = extract_u8_literal(len_expr)?;

          Ok(BitVal::Imp { arg, value })
        }
        other => match BitArg::from_str(other) {
          Ok(bitarg) => Ok(BitVal::Arg(bitarg)),
          Err(_) => Err(syn::Error::new_spanned(ident, format!("No BitArg named {other}"))),
        },
      }
    } else {
      Err(lookahead.error())
    }
  }
}

#[derive(Clone)]
struct InstructionEntry {
  name: Ident,
  exec: Expr,
  pattern: Vec<BitVal>, // Vec<Chunks>
}

fn byte_split_bitval(pat: &Vec<BitVal>) -> Vec<Vec<BitVal>> {
  let mut bytes = vec![];
  let mut byte = vec![];
  let mut len = 0;

  for bitval in pat {
    byte.push(bitval.clone());
    len += bitval.len();
    if len == 8 {
      bytes.push(byte.clone());
      byte = vec![];
      len = 0;
    }
    assert!(len < 8, "Bit pattern is not byte aligned");
  }
  if !byte.is_empty() && len == 0 {
    bytes.last_mut().unwrap().append(&mut byte);
  }
  assert!(len == 0, "Bit pattern is not complete");

  bytes
}

fn first_byte_codes(pat: Vec<BitVal>) -> Vec<u16> {
  let (res, len) = byte_split_bitval(&pat)
    .iter()
    .take(2)
    .flatten()
    .map(|&x| match x {
      BitVal::Lit { pattern, len } => (vec![pattern], len),
      other => ((0..=((1u16 << other.len()) - 1) as u8).into_iter().collect(), other.len()),
    })
    .fold((vec![0], 0), |(acc, la), (xs, lx)| {
      let mut res = vec![];
      for a in &acc {
        for &b in &xs {
          res.push(a << lx | b as u16)
        }
      }
      (res, la + lx as u16)
    });

  if len == 8 {
    res
      .iter()
      .map(|&hi| (0..=u8::MAX).map(|lo| u16::from_be_bytes([hi as u8, lo])).collect::<Vec<_>>())
      .flatten()
      .collect()
  } else {
    res
  }
}

impl Parse for InstructionEntry {
  fn parse(input: ParseStream) -> Result<Self> {
    let content;
    syn::bracketed!(content in input);

    let name: Ident = content.parse()?;
    content.parse::<Token![,]>()?;

    let fn_pointer: Expr = content.parse()?;
    content.parse::<Token![,]>()?;

    let list_content;
    syn::bracketed!(list_content in content);
    let idents =
      syn::punctuated::Punctuated::<BitVal, Token![,]>::parse_terminated(&list_content)?.into_iter().collect();

    Ok(InstructionEntry { name, exec: fn_pointer, pattern: idents })
  }
}

fn generate_instruction_enum(tokens: TokenStream) -> TokenStream {
  let mut var_names = vec![];
  let mut str_names = vec![];
  let mut seen = HashSet::new();
  for data in syn::punctuated::Punctuated::<InstructionEntry, syn::Token![,]>::parse_terminated.parse(tokens).unwrap() {
    let ident = data.name;
    let str_name = ident.to_string();
    if seen.insert(str_name.clone()) {
      var_names.push(quote! {#ident,});
      str_names.push(quote! {#str_name,});
    }
  }
  quote! {
    #[derive(Debug, Clone, Copy)]
    pub enum InstructionKind {
      #(#var_names)*
    }

    impl InstructionKind {
      pub fn to_string(&self) -> String {
        let str_names = vec![#(#str_names)*];
        str_names[*self as usize].to_lowercase().to_owned()
      }
    }
  }
  .into()
}

fn generate_instruction_decode_table(tokens: TokenStream) -> TokenStream {
  let mut entries = vec![];
  let mut content = vec![];
  for instruction in
    syn::punctuated::Punctuated::<InstructionEntry, syn::Token![,]>::parse_terminated.parse(tokens).unwrap()
  {
    let bits = instruction.pattern.clone();
    let vars = first_byte_codes(bits);
    entries.push((instruction.clone(), vars));
  }

  for (entry, ids) in entries {
    let name = entry.name;
    let exec = entry.exec;
    let pattern = entry.pattern;
    let pattern_bytes = byte_split_bitval(&pattern);

    let mut inner_content = vec![];
    let mut hs: HashSet<BitArg> = HashSet::new();
    inner_content.push(quote! {
      let mut bytes_read = 0u16;
      let mut flags = 0u16;
      let mut disp_lo: Option<u8> = None;
      let mut disp_hi: Option<u8> = None;
    });

    for pattern in pattern_bytes.iter() {
      inner_content.push(quote! {
        let b = sa.get_memory(bytes_read)[0];
        bytes_read += 1;
      });

      let mut i = 0;
      for bitval in pattern {
        let rshift = 8 - (i + bitval.len());
        let mask = ((1u16 << bitval.len()) - 1) as u8;
        match bitval {
          BitVal::Lit { pattern, len } => inner_content.push(quote! {
            assert!((b >> #rshift) & #mask == #pattern, "Wanted {:0width$b}, got {:0width$b}"
              , #pattern, (b >> #rshift) & #mask, width=#len as usize);
          }),
          BitVal::Arg(BitArg::DISP_LO) => {
            inner_content.push(quote! {
              disp_lo = if (mod_ == 0b00 && r_m_ == 0b110) || (mod_ == 0b01) || (mod_ == 0b10) { Some(b) } else {
                bytes_read -= 1;
                None
              };
            });
          }
          BitVal::Arg(BitArg::DISP_HI) => {
            inner_content.push(quote! {
              disp_hi = if (mod_ == 0b00 && r_m_ == 0b110) || (mod_ == 0b10) { Some(b) } else {
                bytes_read -= 1;
                None
              };
            });
          }
          BitVal::Arg(BitArg::DATA_HI) => {
            inner_content.push(quote! {
              let data_hi = if w_ == 1 { Some(b) } else {
                bytes_read -= 1;
                None
              };
            });
          }
          BitVal::Arg(BitArg::DATA_HI_IF_SW) => {
            inner_content.push(quote! {
              let data_hi = if s_ == 0 && w_ == 1 { Some(b) } else {
                bytes_read -= 1;
                None
              };
            });
          }
          BitVal::Arg(arg) => {
            hs.insert(*arg);
            let var = arg.to_ident();
            inner_content.push(quote! {
              let #var = (b >> #rshift) & #mask;
            });
          }
          BitVal::Imp { arg, value } => {
            hs.insert(*arg);
            let var = arg.to_ident();
            inner_content.push(quote! {
              let #var = #value;
            });
          }
        }
        i += bitval.len();
      }
    }

    if !hs.contains(&BitArg::D) {
      inner_content.push(quote! {
        let d_ = 0;
      });
    }

    inner_content.push(quote! {
      let mut operands = [Operand::None, Operand::None];
      let mut segment_register = None::<Register>;
      let (first, last) = operands.split_at_mut(1);
      let (reg_operand, mod_operand) = if d_ == 0 {
        (&mut last[0], &mut first[0])
      } else {
        (&mut first[0], &mut last[0])
      };
    });

    if hs.contains(&BitArg::W) {
      inner_content.push(quote! {
        flags |= InstructionFlag::Wide as u16;
      });
      if !hs.contains(&BitArg::W_R_M) {
        inner_content.push(quote! {
          let w_r_m_ = w_;
        })
      }
    }

    for bitarg in &hs {
      match bitarg {
        BitArg::SR => inner_content.push(quote! {
          *reg_operand = Operand::Register(SR_TO_REGISTER[sr_ as usize]);
        }),
        BitArg::REG => inner_content.push(quote! {
          *reg_operand = Operand::Register(REG_TO_REGISTER[reg_ as usize][w_ as usize]);
        }),
        BitArg::MOD => inner_content.push(quote! {
          match mod_  {
            0b11 => {
               *mod_operand = Operand::Register(REG_TO_REGISTER[r_m_ as usize][w_r_m_ as usize]);
            },
            0b00 if r_m_ == 0b110 => {
              let displacement = u16::from_le_bytes([disp_lo.unwrap(), disp_hi.unwrap()]);
              *mod_operand = Operand::Memory(EffectiveAddressExpression::Direct(displacement));
              segment_register = Some(reg::DS);
            },
            0b00 | 0b01 | 0b10 => {
              let displacement = if let Some(disp_hi) = disp_hi {
                i16::from_le_bytes([disp_lo.unwrap(), disp_hi])
              } else if let Some(disp_lo) = disp_lo {
                i8::from_le_bytes([disp_lo]) as i16
              } else {0};
              let terms = R_M_TO_EA_TERMS[r_m_ as usize];
              *mod_operand = Operand::Memory(EffectiveAddressExpression::Expression{
                terms,
                displacement,
              });
              segment_register = Some(R_M_TO_DEFAULT_SEGMENT_REGISTER[r_m_ as usize]);
            }
            _ => panic!("Invalid mod implied value {mod_}"),
          }
        }),
        _ => {}
      }
    }

    if hs.contains(&BitArg::ADDR_LO) {
      inner_content.push(quote! {
        let addr = u16::from_le_bytes([addr_lo_, addr_hi_]);
        let free_operand = if let Operand::None = reg_operand {
          reg_operand
        } else if let Operand::None = mod_operand {
          mod_operand
        } else { panic!("Immediate value but no free operands") };
        *free_operand = Operand::Memory(EffectiveAddressExpression::Direct(addr));
        segment_register = Some(reg::DS);
      });
    }

    if hs.contains(&BitArg::DATA_LO) {
      inner_content.push(if hs.contains(&BitArg::S) {
        quote! {
          let data = if let Some(data_hi) = data_hi {
            Immediate::WordValue(i16::from_le_bytes([data_lo_, data_hi]))
          } else if s_ == 1 && w_ == 1 {
            Immediate::WordValue(data_lo_ as i8 as i16)
          } else {
            Immediate::ByteValue(data_lo_ as i8)
          };
        }
      } else {
        quote! {
            let data = if let Some(data_hi) = data_hi {
              Immediate::WordValue(i16::from_le_bytes([data_lo_, data_hi]))
            } else {
              Immediate::ByteValue(data_lo_ as i8)
            };
        }
      });

      inner_content.push(quote! {
        let free_operand = if let Operand::None = reg_operand {
          reg_operand
        } else if let Operand::None = mod_operand {
          mod_operand
        } else { panic!("Immediate value but no free operands") };
        *free_operand = Operand::Immediate(data);
      });
    }

    if hs.contains(&BitArg::IP8) {
      inner_content.push(quote! {
        let free_operand = if let Operand::None = reg_operand {
          reg_operand
        } else if let Operand::None = mod_operand {
          mod_operand
        } else { panic!("Immediate value but no free operands") };
        *free_operand = Operand::Immediate(Immediate::JumpDisplacement(ip8_ as i8 as i16));
      });
    }

    if hs.contains(&BitArg::DATA8) {
      inner_content.push(quote! {
        let free_operand = if let Operand::None = reg_operand {
          reg_operand
        } else if let Operand::None = mod_operand {
          mod_operand
        } else { panic!("Immediate value but no free operands") };
        *free_operand = Operand::Immediate(Immediate::ByteValue(data8_ as i8));
      });
    }

    let i0 = ids[0] as usize;

    let ids_parts = ids[1..]
      .iter()
      .map(|&i| {
        let i = i as usize;
        quote! {#i, }
      })
      .collect::<Vec<_>>();

    content.push(quote! {
      inner[#i0] = Some(|sa| {
          #(#inner_content)*
          let address = sa.get_absoulute_address_of(0);
          let size = 0;
          let kind = InstructionKind::#name;
          Instruction {
            address,
            flags,
            kind,
            operands,
            size: bytes_read,
            exec: #exec,
            segment_register
          }
        });

      let ids = [#(#ids_parts)*];
      let mut it = 0usize;
      while it < ids.len() {
        inner[ids[it]] = inner[#i0];
        it += 1;
      }
    });
  }

  quote! {
    pub type DecodeTable = [Option<fn(&SegmentedAccess) -> Instruction>; 1 << 16];
    pub const DECODE_TABLE: DecodeTable = {
      let mut inner: [Option<fn(&SegmentedAccess) -> Instruction>; 1 << 16] = [None; 1 << 16];

      #(#content)*

      inner
    };
  }
  .into()
}

#[proc_macro]
pub fn instructions(tokens: TokenStream) -> TokenStream {
  let mut content: TokenStream = quote! { use crate::types::*; }.into();
  content.extend(generate_instruction_enum(tokens.clone()));
  content.extend(generate_instruction_decode_table(tokens.clone()));
  content.into()
}

#[proc_macro_derive(AsRef)]
pub fn derive_as_ref(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);

  let name = input.ident;
  let generics = input.generics;

  // Split generics into parts for impl
  let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

  let expanded = quote! {
      impl #impl_generics AsRef<#name #ty_generics> for #name #ty_generics #where_clause {
          fn as_ref(&self) -> &Self {
              self
          }
      }
  };

  expanded.into()
}

#[cfg(test)]
mod tests {
  use super::*;

  // #[test]
  fn _main() {
    let bits = vec![
      BitVal::Lit { pattern: 0b1011, len: 4 },
      BitVal::Arg(BitArg::W),
      BitVal::Arg(BitArg::REG),
      BitVal::Arg(BitArg::DATA_LO),
      BitVal::Arg(BitArg::DATA_HI),
      BitVal::Imp { arg: BitArg::D, value: 1 },
    ];
    dbg!(&bits);

    let thing = first_byte_codes(bits);
    println!("{}", thing.len() / 256);

    assert!(false);
  }
}
