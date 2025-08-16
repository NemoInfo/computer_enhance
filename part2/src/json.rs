use std::collections::{BTreeMap};

use macros::{parse_either, parse_optional, parse_repeat0, parse_repeat0_sep, parse_repeat1, parse_sequence};

#[derive(Debug, PartialEq)]
pub enum Value {
  Object(BTreeMap<String, Value>),
  Array(Vec<Value>),
  String(String),
  Number(Number),
  True,
  False,
  Null,
}

#[derive(Debug, PartialEq)]
pub enum Number {
  I64(i64),
  F64(f64),
}

const WHITE_SPACE: &[char] = &[' ', '\n', '\r', '\t'];
const ESCAPED_NON_HEX: &[char] = &['"', '\\', '/', 'b', 'f', 'n', 'r', 't'];
const ZERO_TO_NINE: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const SIGN: &[char] = &['+', '-'];

type ParseResult<'a, T> = Result<(&'a str, T), String>;

pub fn parse_json(input: &str) -> Result<Value, String> {
  let (input, json) = parse_element(input)?;
  match input.is_empty() {
    true => Ok(json),
    false => Err("Unexpected trailing characters after JSON value".into()),
  }
}

fn parse_element(input: &str) -> ParseResult<Value> {
  let (mut input, val) = parse_value(input.trim_start_matches(WHITE_SPACE))?;
  input = input.trim_start_matches(WHITE_SPACE);

  Ok((input, val))
}

fn parse_value(input: &str) -> ParseResult<Value> {
  parse_either!(parse_object, parse_array, parse_string, parse_number, parse_true, parse_false, parse_null)(input)
    .map_err(|_| "Could not parse JSON value".into())
}

fn parse_object(input: &str) -> ParseResult<Value> {
  let (input, _) = parse_char('{')(input)?;
  let (mut input, members) = parse_repeat0_sep!(parse_member, parse_char(','))(input)?;
  input = input.trim_start_matches(WHITE_SPACE);
  let (input, _) = parse_char('}')(input)?;

  Ok((input, Value::Object(members.into_iter().collect())))
}

fn parse_member(input: &str) -> ParseResult<(String, Value)> {
  let (input, key) = parse_string(input.trim_start_matches(WHITE_SPACE))?;
  let Value::String(key) = key else { unreachable!() };
  let (input, _) = parse_char(':')(input.trim_start_matches(WHITE_SPACE))?;
  let (input, value) = parse_element(input)?;

  Ok((input, (key, value)))
}

fn parse_array(input: &str) -> ParseResult<Value> {
  let (input, _) = parse_char('[')(input)?;
  let (mut input, elements) = parse_repeat0_sep!(parse_element, parse_char(','))(input)?;
  input = input.trim_start_matches(WHITE_SPACE);
  let (input, _) = parse_char(']')(input)?;

  Ok((input, Value::Array(elements)))
}

fn parse_string(input: &str) -> ParseResult<Value> {
  let (input, _) = parse_char('"')(input)?;
  let (input, characters) = parse_repeat0!(parse_character)(input)?;
  let (input, _) = parse_char('"')(input)?;

  let unescaped = unescape_unicode(&characters)?;

  Ok((input, Value::String(unescaped)))
}

fn parse_character(input: &str) -> ParseResult<&str> {
  parse_either!(parse_non_escaped, parse_escaped)(input).map_err(|_| "Could not parse character".into())
}

fn parse_escaped(input: &str) -> ParseResult<&str> {
  (parse_sequence!(
    parse_char('\\'),
    parse_either!(
      parse_chars(ESCAPED_NON_HEX),
      parse_sequence!(parse_char('u'), parse_hex, parse_hex, parse_hex, parse_hex)
    )
  ))(input)
  .map_err(|_| "Could not parse character".into())
}

fn parse_non_escaped(input: &str) -> ParseResult<&str> {
  input
    .strip_prefix(|c: char| !c.is_control() && !['"', '\\'].contains(&c))
    .map(|x| (x, &input[..1]))
    .ok_or(format!("Could not parse JSON non_escaped"))
}

fn parse_hex(input: &str) -> ParseResult<&str> {
  input
    .strip_prefix(|c: char| c.is_ascii_alphanumeric())
    .map(|x| (x, &input[..1]))
    .ok_or(format!("Could not parse JSON non_escaped"))
}

fn parse_number(input: &str) -> ParseResult<Value> {
  let (input, number) = parse_sequence!(parse_integer, parse_fraction, parse_exponent)(input)?;
  Ok(match number.contains(&['.', 'e', 'E']) {
    true => {
      (input, Value::Number(Number::F64(number.parse().map_err(|_| format!("Could not parse to f64: {number}"))?)))
    }
    false => {
      (input, Value::Number(Number::I64(number.parse().map_err(|_| format!("Could not parse to i64: {number}"))?)))
    }
  })
}

fn parse_integer(input: &str) -> ParseResult<&str> {
  parse_either!(
    parse_sequence!(parse_chars(&ZERO_TO_NINE[1..]), parse_repeat1!(parse_digit)),
    parse_digit,
    parse_sequence!(parse_char('-'), parse_chars(&ZERO_TO_NINE[1..]), parse_repeat1!(parse_digit)),
    parse_sequence!(parse_char('-'), parse_digit)
  )(input)
  .map_err(|_| "Could not parse integer".into())
}

fn parse_fraction(input: &str) -> ParseResult<&str> {
  parse_optional!(parse_sequence!(parse_char('.'), parse_repeat1!(parse_digit)))(input)
}

fn parse_exponent(input: &str) -> ParseResult<&str> {
  parse_optional!(parse_sequence!(parse_char('e'), parse_optional!(parse_chars(SIGN)), parse_repeat1!(parse_digit)))(
    input,
  )
}

fn parse_char(c: char) -> impl FnOnce(&str) -> ParseResult<&str> {
  move |input| input.strip_prefix(c).map(|x| (x, &input[..1])).ok_or(format!("Could not parse JSON char \'{c}\'"))
}

fn parse_chars<'a>(c: &'a [char]) -> impl FnOnce(&str) -> ParseResult<&str> + 'a {
  move |input| input.strip_prefix(c).map(|x| (x, &input[..1])).ok_or(format!("Could not parse JSON chars \"\""))
}

fn parse_digit(input: &str) -> ParseResult<&str> {
  parse_chars(ZERO_TO_NINE)(input).map_err(|_| "Could not parse JSON onenine".into())
}

fn parse_true(input: &str) -> ParseResult<Value> {
  input.strip_prefix("true").map(|x| (x, Value::True)).ok_or("Could not parse JSON true value".into())
}

fn parse_false(input: &str) -> ParseResult<Value> {
  input.strip_prefix("false").map(|x| (x, Value::False)).ok_or("Could not parse JSON false value".into())
}

fn parse_null(input: &str) -> ParseResult<Value> {
  input.strip_prefix("null").map(|x| (x, Value::Null)).ok_or("Could not parse JSON null value".into())
}

mod macros {
  macro_rules! parse_either {
    ( $first:expr, $( $rest:expr ),* ) => {|input| -> Result<_, String> {
        $first(input)$(.or($rest(input)))*
    }};
  }

  macro_rules! parse_sequence {
    ( $first:expr, $( $rest:expr ),* ) => {|input| -> Result<_, String> {
      $first(input)$(.and_then(|(_input, _)| $rest(_input)))*
        .map(|(after, _)| (after, &input[..input.len() - after.len()]))
    }};
  }

  macro_rules! parse_optional {
    ( $parser:expr ) => {
      |input| -> Result<_, String> { $parser(input).or(Ok((input, &input[..0]))) }
    };
  }

  macro_rules! parse_repeat1 {
    ( $parser:expr ) => {
      |input| -> Result<_, String> {
        let (mut after, _) = $parser(input)?;
        while let Ok((it, _)) = $parser(after) {
          after = it;
        }
        Ok((after, &input[..input.len() - after.len()]))
      }
    };
  }

  macro_rules! parse_repeat0 {
    ( $parser:expr ) => {
      parse_optional!(parse_repeat1!($parser))
    };
  }

  macro_rules! parse_repeat0_sep {
    ( $parser:expr, $sep:expr ) => {
      |mut input| -> Result<(&str, Vec<_>), String> {
        let mut res = vec![];
        while let Ok((after, v)) = $parser(input) {
          res.push(v);
          input = after;
          let Ok((after, _)) = $sep(input) else { break };
          input = after;
        }
        Ok((input, res))
      }
    };
  }

  pub(crate) use parse_either;
  pub(crate) use parse_optional;
  pub(crate) use parse_repeat0;
  pub(crate) use parse_repeat0_sep;
  pub(crate) use parse_repeat1;
  pub(crate) use parse_sequence;
}

fn unescape_unicode(input: &str) -> Result<String, String> {
  let mut output = String::with_capacity(input.len());
  let mut chars = input.chars().peekable();

  while let Some(c) = chars.next() {
    if c == '\\' {
      match chars.next() {
        Some('u') => {
          // Parse \uXXXX
          let hex: String = chars.by_ref().take(4).collect();
          if hex.len() != 4 {
            return Err("Invalid \\u escape: not enough digits".into());
          }
          let u = u16::from_str_radix(&hex, 16).map_err(|_| "Invalid \\u escape: bad hex".to_string())?;

          if (0xD800..=0xDBFF).contains(&u) {
            // High surrogate, must have low surrogate next
            if chars.next() != Some('\\') || chars.next() != Some('u') {
              return Err("Expected low surrogate after high surrogate".into());
            }
            let low_hex: String = chars.by_ref().take(4).collect();
            if low_hex.len() != 4 {
              return Err("Invalid low surrogate".into());
            }
            let low = u16::from_str_radix(&low_hex, 16).map_err(|_| "Invalid low surrogate hex".to_string())?;
            if !(0xDC00..=0xDFFF).contains(&low) {
              return Err("Invalid low surrogate value".into());
            }
            let codepoint = 0x10000 + (((u as u32 - 0xD800) << 10) | (low as u32 - 0xDC00));
            output.push(std::char::from_u32(codepoint).ok_or("Invalid Unicode codepoint")?);
          } else {
            // Normal BMP character
            output.push(std::char::from_u32(u as u32).ok_or("Invalid Unicode codepoint")?);
          }
        }
        Some('n') => output.push('\n'),
        Some('r') => output.push('\r'),
        Some('t') => output.push('\t'),
        Some('b') => output.push('\x08'),
        Some('f') => output.push('\x0C'),
        Some('"') => output.push('"'),
        Some('\\') => output.push('\\'),
        Some('/') => output.push('/'),
        Some(c) => return Err(format!("Unknown escape sequence: \\{}", c)),
        None => return Err("Trailing backslash".into()),
      }
    } else {
      output.push(c);
    }
  }

  Ok(output)
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_parse_int() {
    assert_eq!(parse_json("null"), Ok(Value::Null));
    assert_eq!(parse_json(" true \n"), Ok(Value::True));
    assert_eq!(parse_json("\r\t false"), Ok(Value::False));
    assert!(parse_json("\r\t False").is_err());
    dbg!(&parse_json("12901 "));
    dbg!(&parse_json("129012137128371283728173812738127381738127381738178173817381 bla bla"));
    dbg!(&parse_json("0101 "));
    dbg!(&parse_json("-1123"));
    dbg!(&parse_json("10e3 "));
    dbg!(&parse_json("-1.2e-3 "));
    dbg!(&parse_string("\"hahahh \\nehe\""));
    dbg!(&parse_string("\"Hi \\uaC40 \\uD834\\uDD1E\""));
    dbg!(&parse_array("[ 1 , 2   ,\t -1.2e-3, \"h \\n \\bello\"]"));
    dbg!(&parse_object(r#"{"key": "value", "key_dos": -69}"#));
    assert!(false);
  }
}
