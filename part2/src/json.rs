use std::{collections::BTreeMap, fmt::Display, path::Path};

use macros::*;

#[derive(Debug, PartialEq)]
pub enum Value {
  Object(BTreeMap<String, Value>),
  Array(Vec<Value>),
  String(String),
  Number(Number),
  Bool(bool),
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
const HEX: &[char] =
  &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'];
const SIGN: &[char] = &['+', '-'];

#[derive(Debug)]
pub struct ParseError {
  msg: String,
  position: usize,
  children: Vec<ParseError>,
}

impl ParseError {
  fn new(msg: impl Into<String>) -> Self {
    Self { msg: msg.into(), position: 0, children: vec![] }
  }

  fn wrap(self, msg: &str, position: usize) -> ParseError {
    ParseError { msg: msg.to_string(), position, children: vec![self] }
  }

  fn with_pos(self, position: usize) -> ParseError {
    ParseError { position, ..self }
  }

  fn subtract_pos(&mut self, position: usize) {
    self.position -= position;
    for child in &mut self.children {
      child.subtract_pos(position);
    }
  }

  fn relative_pos(mut self) -> Self {
    for child in &mut self.children {
      child.subtract_pos(self.position)
    }
    self.position = 0;
    self
  }

  fn best(&self) -> &Self {
    if self.children.is_empty() {
      return &self;
    }
    let mut best_child = self.children[0].best();
    for child in &self.children[1..] {
      let child_best_child = child.best();
      if child_best_child.position >= best_child.position {
        best_child = child_best_child;
      }
    }

    best_child
  }
}

impl Display for ParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{} at byte {}{}\n{}",
      self.msg,
      self.position,
      ["", ":"][(self.children.len() != 0) as usize],
      self
        .children
        .iter()
        .map(|x| x.to_string().lines().map(|line| format!("  {line}")).collect::<Vec<_>>().join("\n"))
        .collect::<Vec<_>>()
        .join("\n")
    )
  }
}

impl<T> From<T> for ParseError
where
  T: AsRef<str>,
{
  fn from(s: T) -> Self {
    Self::new(s.as_ref())
  }
}

type ParseResult<'a, T> = Result<(&'a str, T), ParseError>;

trait ParseResultExt<T> {
  fn or_accumulate(self, other: impl FnOnce() -> Self) -> Self;
  fn wrap_err(self, msg: &str, position: usize) -> Self;
}

impl<T> ParseResultExt<T> for Result<T, ParseError> {
  fn or_accumulate(self, other: impl FnOnce() -> Self) -> Self {
    match self {
      Ok(val) => Ok(val),
      Err(mut e1) => match other() {
        Ok(val) => Ok(val),
        Err(e2) => {
          let parent = ParseError {
            msg: e1.msg,
            position: e1.position,
            children: {
              e1.children.push(e2);
              e1.children
            },
          };
          Err(parent)
        }
      },
    }
  }

  fn wrap_err(self, msg: &str, position: usize) -> Self {
    self.map_err(|err| err.wrap(msg, position))
  }
}

pub fn parse_json_file<P: AsRef<Path>>(path: P) -> Result<Value, String> {
  let path = path.as_ref();
  let input = std::fs::read_to_string(path).map_err(|e| format!("Could not read file: {e}"))?;
  let (input, json) = parse_element(&input).map_err(|e| {
    let e = e.relative_pos();
    let position = e.best().position;
    let (line, col) = line_col(&input, position);
    let msg = e.best().msg.clone();

    ParseError {
      msg: format!(
        "[{}:{line}:{col} \x1b[31merror\x1b[0m] {}\n{}\n{}^",
        path.to_string_lossy(),
        msg,
        input.lines().nth(line - 1).unwrap(),
        " ".repeat(col - 1),
      ),
      children: vec![],
      position,
    }
    .to_string()
  })?;
  match input.is_empty() {
    true => Ok(json),
    false => Err("Unexpected trailing characters after JSON value".into()), // TODO
  }
}

pub fn _parse_json(input: &str) -> Result<Value, String> {
  let (input, json) = parse_element(input).map_err(|e| {
    let e = e.relative_pos();
    let position = e.best().position;
    let (line, col) = line_col(input, position);
    let msg = e.best().msg.clone();

    ParseError {
      msg: format!("{}\n{}\n{}", input.lines().nth(line - 1).unwrap(), " ".repeat(col - 1), msg),
      children: vec![],
      position,
    }
    .to_string()
  })?;
  match input.is_empty() {
    true => Ok(json),
    false => Err("Unexpected trailing characters after JSON value".into()), // TODO
  }
}

fn parse_element(input: &str) -> ParseResult<Value> {
  let (mut input, val) = parse_value(input.trim_json())?;
  input = input.trim_json();

  Ok((input, val))
}

fn parse_value(input: &str) -> ParseResult<Value> {
  parse_either!(parse_object, parse_array, parse_string, parse_number, parse_bool, parse_null)(input).map_err(|err| {
    ParseError { msg: "Could not match JSON value variants".into(), position: input.as_ptr() as usize, ..err }
  })
}

fn parse_object(input: &str) -> ParseResult<Value> {
  (|| {
    let (input, _) = match_char('{')(input)?;
    let (input, members) = parse_repeat0_sep_end!(parse_member, match_char(','), match_char('}'))(input.trim_json())?;

    Ok((input, Value::Object(members.into_iter().collect())))
  })()
  .wrap_err("Could not parse JSON object", input.as_ptr() as usize)
}

fn parse_member(input: &str) -> ParseResult<(String, Value)> {
  let (input, key) = parse_string(input.trim_json())?;
  let Value::String(key) = key else { unreachable!() };
  let (input, _) = match_char(':')(input.trim_json())?;
  let (input, value) = parse_element(input)?;

  Ok((input, (key, value)))
}

fn parse_array(input: &str) -> ParseResult<Value> {
  (|| {
    let (input, _) = match_char('[')(input)?;
    let (input, elements) = parse_repeat0_sep_end!(parse_element, match_char(','), match_char(']'))(input)?;

    Ok((input, Value::Array(elements)))
  })()
  .wrap_err("Could not parse JSON array", input.as_ptr() as usize)
}

fn parse_string(input: &str) -> ParseResult<Value> {
  (|| {
    let (input, _) = match_char('"')(input)?;
    let (input, characters) = match_repeat0_end!(parse_character, match_char('"'))(input)?;

    let unescaped = unescape_unicode(&characters)
      .map_err(|(msg, i)| ParseError::from(msg).with_pos(input.as_ptr() as usize + i - characters.len()))?;

    Ok((input, Value::String(unescaped)))
  })()
  .wrap_err("Could not parse JSON string", input.as_ptr() as usize)
}

fn parse_character(input: &str) -> ParseResult<&str> {
  parse_either!(parse_non_escaped, parse_escaped)(input).map_err(|err| ParseError {
    msg: "Could not parse any JSON character variant".into(),
    position: input.as_ptr() as usize,
    ..err
  })
}

fn parse_escaped(input: &str) -> ParseResult<&str> {
  (match_sequence!(
    match_char('\\'),
    parse_either!(
      match_chars(ESCAPED_NON_HEX),
      match_sequence!(match_char('u'), match_hex, match_hex, match_hex, match_hex)
    )
  ))(input)
  .wrap_err("Could not parse JSON espcaped characther", input.as_ptr() as usize)
}

fn parse_non_escaped(input: &str) -> ParseResult<&str> {
  input
    .strip_prefix(|c: char| !c.is_control() && !['"', '\\'].contains(&c))
    .map(|x| (x, &input[..1]))
    .ok_or(ParseError::from("Could not parse JSON non_escaped characther").with_pos(input.as_ptr() as usize))
}

#[inline]
fn match_hex(input: &str) -> ParseResult<&str> {
  match_chars(HEX)(input).map_err(|_| ParseError::from("Could not match JSON hex").with_pos(input.as_ptr() as usize))
}

fn parse_number(input: &str) -> ParseResult<Value> {
  (|| {
    let (input, number) = match_sequence!(match_integer, match_fraction, match_exponent)(input)?;
    Ok((
      input,
      Value::Number(match number.contains(&['.', 'e', 'E']) {
        true => Number::F64(number.parse().map_err(|_| {
          ParseError::from(format!("Could not parse to f64: {number}")).with_pos(input.as_ptr() as usize)
        })?),
        false => Number::I64(number.parse().map_err(|_| {
          ParseError::from(format!("Could not parse to i64: {number}")).with_pos(input.as_ptr() as usize)
        })?),
      }),
    ))
  })()
  .wrap_err("Could not parse JSON number", input.as_ptr() as usize)
}

fn match_integer(input: &str) -> ParseResult<&str> {
  parse_either!(
    match_sequence!(match_onezero, match_digits),
    match_digit,
    match_sequence!(match_char('-'), match_onezero, match_digits),
    match_sequence!(match_char('-'), match_digit)
  )(input)
  .map_err(|err| ParseError { msg: "Could not match JSON integer variants".into(), ..err })
}

fn match_fraction(input: &str) -> ParseResult<&str> {
  match_optional!(match_sequence!(match_char('.'), match_digits))(input)
}

fn match_exponent(input: &str) -> ParseResult<&str> {
  match_optional!(match_sequence!(match_char('e'), match_optional!(match_chars(SIGN)), match_digits))(input)
}

#[inline(always)]
fn match_char(c: char) -> impl FnOnce(&str) -> ParseResult<&str> {
  move |input| {
    input.strip_prefix(c).map(|x| (x, &input[..1])).ok_or_else(|| {
      ParseError::from(format!("Expected {c:?}, got {:?}", &input[..1.min(input.len())]))
        .with_pos(input.as_ptr() as usize)
    })
  }
}

#[inline(always)]
fn match_chars<'a>(c: &'a [char]) -> impl FnOnce(&str) -> ParseResult<&str> + 'a {
  move |input| {
    input.strip_prefix(c).map(|x| (x, &input[..1])).ok_or_else(|| {
      ParseError::from(format!("Expected \'{c:?}\', got \'{}\'", &input[..1.min(input.len())]))
        .with_pos(input.as_ptr() as usize)
    })
  }
}

#[inline(always)]
fn match_str(s: &str) -> impl FnOnce(&str) -> ParseResult<&str> + '_ {
  move |input| {
    input
      .strip_prefix(s)
      .map(|x| (x, &input[..s.len()]))
      .ok_or(ParseError::from(format!("Could not match str {s:?}")).with_pos(input.as_ptr() as usize))
  }
}

#[inline]
fn match_onezero(input: &str) -> ParseResult<&str> {
  match_chars(&ZERO_TO_NINE[1..])(input)
    .map_err(|_| ParseError::from("Could not parse JSON onezero").with_pos(input.as_ptr() as usize))
}

#[inline]
fn match_digit(input: &str) -> ParseResult<&str> {
  match_chars(ZERO_TO_NINE)(input)
    .map_err(|_| ParseError::from("Could not parse JSON digit").with_pos(input.as_ptr() as usize))
}

fn match_digits(input: &str) -> ParseResult<&str> {
  match_repeat1!(match_digit)(input)
    .map_err(|_| ParseError::from("Could not parse JSON digits").with_pos(input.as_ptr() as usize))
}

fn parse_bool(input: &str) -> ParseResult<Value> {
  let (input, boolean) = parse_either!(match_str("true"), match_str("false"))(input)
    .map_err(|_| ParseError::from("Could not parse JSON bool").with_pos(input.as_ptr() as usize))?;
  Ok((input, Value::Bool(boolean.parse().unwrap())))
}

fn parse_null(input: &str) -> ParseResult<Value> {
  match_str("null")(input).map(|(input, _)| (input, Value::Null))
}

mod macros {
  macro_rules! parse_either {
    ( $first:expr, $( $rest:expr ),* ) => {|input| -> ParseResult<_> {
        $first(input).
          or_else(|err| Err(ParseError { position: input.as_ptr() as usize, msg: "either did not match any variant".into(), children: vec![err] }))
          $(.or_accumulate(|| $rest(input)))*
    }};
  }

  macro_rules! parse_repeat1_sep_end {
    ( $parser:expr, $sep:expr, $end:expr ) => {
      |mut input| -> ParseResult<Vec<_>> {
        let mut res = vec![];
        loop {
          let (after, v) = $parser(input)?;
          res.push(v);
          input = after;

          if let Ok((after, _)) = $end(input) {
            return Ok((after, res));
          }

          let (after, _) = $sep(input)?;
          input = after;
        }
      }
    };
  }

  macro_rules! parse_repeat0_sep_end {
    ( $parser:expr, $sep:expr, $end:expr ) => {
      |input| -> ParseResult<Vec<_>> {
        parse_repeat1_sep_end!($parser, $sep, $end)(input)
          .or_else(|err| {
            Err(ParseError {
              position: input.as_ptr() as usize,
              msg: "either did not match any variant".into(),
              children: vec![err],
            })
          })
          .or_accumulate(|| $end(input).map(|(input, _)| (input, vec![])))
      }
    };
  }

  macro_rules! _parse_optional {
    ( $parser:expr ) => {
      |input| -> ParseResult<Option<_>> {
        $parser(input).map(|(input, res)| (input, Some(res))).or_else(|_| Ok((input, None)))
      }
    };
  }

  macro_rules! match_sequence {
    ( $first:expr, $( $rest:expr ),* ) => {|input| -> ParseResult<_> {
      $first(input)$(.and_then(|(input, _)| $rest(input)))*
        .map(|(after, _)| (after, &input[..input.len() - after.len()]))
    }};
  }

  macro_rules! match_repeat1 {
    ( $parser:expr ) => {
      |input| -> ParseResult<_> {
        let (mut after, _) = $parser(input)?;
        while let Ok((it, _)) = $parser(after) {
          after = it;
        }
        Ok((after, &input[..input.len() - after.len()]))
      }
    };
  }

  macro_rules! _match_repeat1_end {
    ( $parser:expr, $end:expr ) => {
      |mut input| -> ParseResult<_> {
        loop {
          let (after, _) = $parser(input)?;
          input = after;

          if let Ok((after,)) = $end(input) {
            return Ok((after, &input[..input.len() - after.len() - 1]));
          }
        }
      }
    };
  }

  macro_rules! match_repeat0_end {
    ( $parser:expr, $end:expr ) => {
      |input| -> ParseResult<_> {
        let mut next = input;
        loop {
          if let Ok((after, _)) = $end(next) {
            return Ok((after, &input[..input.len() - after.len() - 1]));
          }

          let (after, _) = $parser(next)?;
          next = after;
        }
      }
    };
  }

  macro_rules! _match_repeat0 {
    ( $parser:expr ) => {
      match_optional!(match_repeat1!($parser))
    };
  }

  macro_rules! match_optional {
    ( $parser:expr ) => {
      |input| -> ParseResult<_> { $parser(input).or(Ok((input, &input[..0]))) }
    };
  }

  // pub(crate) use _match_repeat0;
  // pub(crate) use _parse_optional;
  pub(crate) use match_optional;
  pub(crate) use match_repeat0_end;
  pub(crate) use match_repeat1;
  pub(crate) use match_sequence;
  pub(crate) use parse_either;
  pub(crate) use parse_repeat0_sep_end;
  pub(crate) use parse_repeat1_sep_end;
}

fn unescape_unicode(input: &str) -> Result<String, (String, usize)> {
  let mut output = String::with_capacity(input.len());
  let mut chars = input.chars().peekable();

  let mut i = 0;
  while let Some(c) = chars.next() {
    i += 1;
    if c == '\\' {
      i += 1; // TODO check off by one
      match chars.next() {
        Some('u') => {
          // Parse \uXXXX
          let hex: String = chars.by_ref().take(4).collect();
          if hex.len() != 4 {
            return Err(("Invalid \\u escape: not enough digits".into(), i));
          }
          let u = u16::from_str_radix(&hex, 16).map_err(|_| ("Invalid \\u escape: bad hex".to_string(), i))?;

          if (0xD800..=0xDBFF).contains(&u) {
            // High surrogate, must have low surrogate next
            if chars.next() != Some('\\') || chars.next() != Some('u') {
              return Err(("Expected low surrogate after high surrogate".into(), i));
            }
            let low_hex: String = chars.by_ref().take(4).collect();
            if low_hex.len() != 4 {
              return Err(("Invalid low surrogate".into(), i));
            }
            let low = u16::from_str_radix(&low_hex, 16).map_err(|_| ("Invalid low surrogate hex".to_string(), i))?;
            if !(0xDC00..=0xDFFF).contains(&low) {
              return Err(("Invalid low surrogate value".into(), i));
            }
            let codepoint = 0x10000 + (((u as u32 - 0xD800) << 10) | (low as u32 - 0xDC00));
            output.push(std::char::from_u32(codepoint).ok_or(("Invalid Unicode codepoint".to_string(), i))?);
          } else {
            // Normal BMP character
            output.push(std::char::from_u32(u as u32).ok_or(("Invalid Unicode codepoint".to_string(), i))?);
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
        Some(c) => return Err((format!("Unknown escape sequence: \\{}", c), i)),
        None => return Err(("Trailing backslash".into(), i)),
      }
    } else {
      output.push(c);
    }
  }

  Ok(output)
}

pub trait TrimJson {
  fn trim_json(&self) -> &str;
}

impl TrimJson for str {
  fn trim_json(&self) -> &str {
    self.trim_start_matches(WHITE_SPACE)
  }
}

fn line_col(s: &str, byte_index: usize) -> (usize, usize) {
  let mut line = 1;
  let mut col = 1;
  for (i, ch) in s.char_indices() {
    if i == byte_index {
      break;
    }
    match ch {
      '\n' => {
        line += 1;
        col = 1;
      }
      // '\r' => continue, // ignore carriage return
      _ => col += 1,
    }
  }
  (line, col)
}

#[cfg(test)]
mod test {
  #[test]
  fn test_parse_int() {
    //   assert_eq!(parse_json("null"), Ok(Value::Null));
    //   assert_eq!(parse_json(" true \n"), Ok(Value::Bool(true)));
    //   assert_eq!(parse_json("\r\t false"), Ok(Value::Bool(false)));
    //   assert!(parse_json("\r\t False").is_err());
    //   dbg!(&parse_json("12901 "));
    //   dbg!(&parse_json("129012137128371283728173812738127381738127381738178173817381 bla bla"));
    //   dbg!(&parse_json("0101 "));
    //   dbg!(&parse_json("-1123"));
    //   dbg!(&parse_json("10e3 "));
    //   dbg!(&parse_json("-1.2e-3 "));
    //   dbg!(&parse_string("\"hahahh \\nehe\""));
    //   dbg!(&parse_string("\"Hi \\uaC40 \\uD834\\uDD1E\""));
    //   dbg!("{}", &parse_json("[ 1 , 2   ,\t -1.2e-3, \"h \\n \\bello\" \t ]"));
    //   dbg!(&parse_object(r#"{"key": "value", "key_dos": -69}"#));
    //   println!("=====\n{}\n======", parse_json("{\"key\": true, \"thing\\uD801\": -1.2e-5  \n }").unwrap_err());
    //   println!("=====\n{:?}\n======", parse_json(r#" {}"#));
    //   println!("=====\n{}\n======", parse_json(r#"   {"key\u035G": true }"#).unwrap_err());
    //   assert!(false);
  }
}
