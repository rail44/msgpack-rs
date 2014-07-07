#![feature(phase)]
#![feature(macro_rules)]

#[phase(plugin, link)]
extern crate log;

extern crate serialize;

use std::collections::HashMap;
use std::io::IoResult;
use std::io::BufReader;
use std::iter;
use serialize::{
  Decodable,
};

macro_rules! expect(
  ($e:expr, Nil) => {
    try!(
      match $e {
        Nil => Ok(()),
        other => Err(ExpectedError("Null".to_string(), format!("{}", other)))
      }
    )
  };
  ($e:expr, $t:ident) => {
    try!(
      match $e {
        $t(v) => Ok(v),
        other => {
          Err(ExpectedError(stringify!($t).to_string(),
          format!("{}", other)))
        }
      }
    )
  };
  ($e:expr, $t:ident, $($t_rest:ident),+) => {
    expect!(expect!($e, $t), $($t_rest),+)
  }
)

#[deriving(Show, Clone, PartialEq)]
pub enum ErrorCode {
  InvalidSyntax,
  InvalidNumber,
  EOFWhileParsingObject,
  EOFWhileParsingList,
  EOFWhileParsingValue,
  EOFWhileParsingString,
  KeyMustBeAString,
  ExpectedColon,
  TrailingCharacters,
  InvalidEscape,
  InvalidUnicodeCodePoint,
  LoneLeadingSurrogateInHexEscape,
  UnexpectedEndOfHexEscape,
  UnrecognizedHex,
  NotFourDigit,
  NotUtf8,
}

#[deriving(Clone, PartialEq, Show)]
pub enum MsgPack {
  Integer(IntegerValue),
  Nil,
  Boolean(bool),
  Float(FloatValue),
  String(String),
  Binary(Vec<u8>),
  Array(Vec<MsgPack>),
  Map(HashMap<String, MsgPack>),
  Extended((i8, Vec<u8>))
}

#[deriving(Clone, PartialEq, Show)]
pub enum IntegerValue {
  Int8(i8),
  Int16(i16),
  Int32(i32),
  Int64(i64),
  Uint8(u8),
  Uint16(u16),
  Uint32(u32),
  Uint64(u64)
}

#[deriving(Clone, PartialEq, Show)]
pub enum FloatValue {
  Float32(f32),
  Float64(f64)
}

pub fn from_bytes(b: &[u8]) -> IoResult<MsgPack> {
  let reader = BufReader::new(b);
  let mut parser = Parser::new(reader);
  parser.parse()
}

pub fn from_str(s: &str) -> IoResult<MsgPack> {
  from_bytes(s.as_bytes())
}

// Parser
pub struct Parser<T> {
  rdr: T,
}

impl<T: Reader> Parser<T> {
  pub fn new(rdr: T) -> Parser<T> {
    Parser { rdr: rdr }
  }

  pub fn parse(&mut self) -> IoResult<MsgPack> {
    let msgpack = match self.rdr.read_byte() {
      Ok(b) => {
        match b {
          0x00 .. 0x7f => Integer(Uint8(b as u8)),
          0x80 .. 0x8f => Map(try!(self.read_map_data((b - 0x80) as uint))),
          0x90 .. 0x9f => Array(try!(self.read_array_data((b - 0x90) as uint))),
          0xa0 .. 0xbf => String(try!(self.read_bytes((b - 0xa0) as uint).map(|b| String::from_utf8(b).ok().unwrap()))),
          0xc0 => Nil,

          0xc2 => Boolean(false),
          0xc3 => Boolean(true),

          0xc4 => Binary(try!(self.read_bin8())),
          0xc5 => Binary(try!(self.read_bin16())),
          0xc6 => Binary(try!(self.read_bin32())),

          0xc7 => Extended(try!(self.read_ext8())),
          0xc8 => Extended(try!(self.read_ext16())),
          0xc9 => Extended(try!(self.read_ext32())),

          0xca => Float(Float32(try!(self.rdr.read_be_f32()))),
          0xcb => Float(Float64(try!(self.rdr.read_be_f64()))),

          0xcc => Integer(Uint8(try!(self.rdr.read_u8()))),
          0xcd => Integer(Uint16(try!(self.rdr.read_be_u16()))),
          0xce => Integer(Uint32(try!(self.rdr.read_be_u32()))),
          0xcf => Integer(Uint64(try!(self.rdr.read_be_u64()))),

          0xd0 => Integer(Int8(try!(self.rdr.read_i8()))),
          0xd1 => Integer(Int16(try!(self.rdr.read_be_i16()))),
          0xd2 => Integer(Int32(try!(self.rdr.read_be_i32()))),
          0xd3 => Integer(Int64(try!(self.rdr.read_be_i64()))),

          0xd4 => Extended(try!(self.read_fixext1())),
          0xd5 => Extended(try!(self.read_fixext2())),
          0xd6 => Extended(try!(self.read_fixext4())),
          0xd7 => Extended(try!(self.read_fixext8())),
          0xd8 => Extended(try!(self.read_fixext16())),

          0xd9 => String(try!(self.read_bin8().map(|b| String::from_utf8(b).ok().unwrap()))),
          0xda => String(try!(self.read_bin16().map(|b| String::from_utf8(b).ok().unwrap()))),
          0xdb => String(try!(self.read_bin32().map(|b| String::from_utf8(b).ok().unwrap()))),

          0xdc => Array(try!(self.read_array16())),
          0xdd => Array(try!(self.read_array32())),

          0xde => Map(try!(self.read_map16())),
          0xdf => Map(try!(self.read_map32())),

          0xe0 .. 0xff => Integer(Int8(b as i8)),

          _ => Nil
        }
      }
      Err(e) => return Err(e) 
    };
    Ok(msgpack)
  }

  fn read_bytes(&mut self, min: uint) -> IoResult<Vec<u8>> {
    let mut buf = Vec::with_capacity(min);
    unsafe { buf.set_len(min); }
    try!(self.rdr.read_at_least(min, buf.as_mut_slice()));
    Ok(buf.to_owned())
  }

  fn read_ext8(&mut self) -> IoResult<(i8, Vec<u8>)> {
    let min = try!(self.rdr.read_u8());
    let type_id = try!(self.rdr.read_i8());
    let data = try!(self.read_bytes(min as uint));
    Ok((type_id, data))
  }

  fn read_ext16(&mut self) -> IoResult<(i8, Vec<u8>)> {
    let min = try!(self.rdr.read_be_u16());
    let type_id = try!(self.rdr.read_i8());
    let data = try!(self.read_bytes(min as uint));
    Ok((type_id, data))
  }

  fn read_ext32(&mut self) -> IoResult<(i8, Vec<u8>)> {
    let min = try!(self.rdr.read_be_u32());
    let type_id = try!(self.rdr.read_i8());
    let data = try!(self.read_bytes(min as uint));
    Ok((type_id, data))
  }

  fn read_bin8(&mut self) -> IoResult<Vec<u8>> {
    let min = try!(self.rdr.read_u8());
    self.read_bytes(min as uint)
  }

  fn read_bin16(&mut self) -> IoResult<Vec<u8>> {
    let min = try!(self.rdr.read_be_u16());
    self.read_bytes(min as uint)
  }

  fn read_bin32(&mut self) -> IoResult<Vec<u8>> {
    let min = try!(self.rdr.read_be_u32());
    self.read_bytes(min as uint)
  }

  fn read_fixext1(&mut self) -> IoResult<(i8, Vec<u8>)> {
    let type_id = try!(self.rdr.read_i8());
    let data = try!(self.read_bytes(1));
    Ok((type_id, data))
  }

  fn read_fixext2(&mut self) -> IoResult<(i8, Vec<u8>)> {
    let type_id = try!(self.rdr.read_i8());
    let data = try!(self.read_bytes(2));
    Ok((type_id, data))
  }

  fn read_fixext4(&mut self) -> IoResult<(i8, Vec<u8>)> {
    let type_id = try!(self.rdr.read_i8());
    let data = try!(self.read_bytes(4));
    Ok((type_id, data))
  }

  fn read_fixext8(&mut self) -> IoResult<(i8, Vec<u8>)> {
    let type_id = try!(self.rdr.read_i8());
    let data = try!(self.read_bytes(8));
    Ok((type_id, data))
  }

  fn read_fixext16(&mut self) -> IoResult<(i8, Vec<u8>)> {
    let type_id = try!(self.rdr.read_i8());
    let data = try!(self.read_bytes(16));
    Ok((type_id, data))
  }

  fn read_array_data(&mut self, n: uint) -> IoResult<Vec<MsgPack>> {
    let range = iter::range(0, n);
    let mut itr = range.map(|_| self.parse());
    let mut vec = Vec::new();
    for result in itr {
      vec.push(try!(result))
    }
    Ok(vec)
  }

  fn read_array16(&mut self) -> IoResult<Vec<MsgPack>> {
    let n = try!(self.rdr.read_be_u16());
    self.read_array_data(n as uint)
  }

  fn read_array32(&mut self) -> IoResult<Vec<MsgPack>> {
    let n = try!(self.rdr.read_be_u32());
    self.read_array_data(n as uint)
  }

  fn read_map_data(&mut self, n: uint) -> IoResult<HashMap<String, MsgPack>> {
    let mut itr = iter::range(0, 2*n).map(|_| self.parse());
    let mut map_itr = iter::range(0, n);
    let mut map = HashMap::new();
    for _ in map_itr {
      let key = try!(itr.next().unwrap());
      let value = try!(itr.next().unwrap());
      match key {
        String(s) => {
          map.insert(s, value);
        }
        _ => {
          warn!("Map is now only supported with String key");
        }
      }
    }
    Ok(map)
  }

  fn read_map16(&mut self) -> IoResult<HashMap<String, MsgPack>> {
    let n = try!(self.rdr.read_be_u16());
    self.read_map_data(n as uint)
  }

  fn read_map32(&mut self) -> IoResult<HashMap<String, MsgPack>> {
    let n = try!(self.rdr.read_be_u32());
    self.read_map_data(n as uint)
  }
}

#[deriving(Clone, PartialEq, Show)]
pub enum DecoderError {
  NotSupportedError,
  ExpectedError(String, String),
  MissingFieldError(String),
}

// Decoder
pub struct Decoder {
  stack: Vec<MsgPack>,
}

type DecodeResult<T> = Result<T, DecoderError>;

impl Decoder {
  pub fn new(msgpack: MsgPack) -> Decoder {
    Decoder { stack: vec![msgpack] }
  }

  fn pop(&mut self) -> MsgPack {
    self.stack.pop().unwrap()
  }
}


pub fn decode<T: Decodable<Decoder, DecoderError>>(s: &[u8]) -> Result<T, DecoderError> {
  let msgpack = match from_bytes(s) {
    Ok(x) => x,
    Err(e) => fail!("{}",e)
  };

  let mut decoder = Decoder::new(msgpack);
  Decodable::decode(&mut decoder)
}

impl serialize::Decoder<DecoderError> for Decoder {
  fn read_nil(&mut self) -> DecodeResult<()> { Ok(expect!(self.pop(), Nil)) }
  
  fn read_u64(&mut self) -> DecodeResult<u64> { Ok(expect!(self.pop(), Integer, Uint64)) }
  fn read_u32(&mut self) -> DecodeResult<u32> { Ok(expect!(self.pop(), Integer, Uint32)) }
  fn read_u16(&mut self) -> DecodeResult<u16> { Ok(expect!(self.pop(), Integer, Uint16)) }
  fn read_u8(&mut self) -> DecodeResult<u8> { Ok(expect!(self.pop(), Integer, Uint8)) }
  fn read_uint(&mut self) -> DecodeResult<uint> {
    match expect!(self.pop(), Integer) {
      Uint64(value) => Ok(value as uint),
      Uint32(value) => Ok(value as uint),
      Uint16(value) => Ok(value as uint),
      Uint8(value) => Ok(value as uint),
      value => Err(ExpectedError("Uint".to_string(), format!("{}", value)))
    }
  }

  fn read_i64(&mut self) -> DecodeResult<i64> { Ok(expect!(self.pop(), Integer, Int64)) }
  fn read_i32(&mut self) -> DecodeResult<i32> { Ok(expect!(self.pop(), Integer, Int32)) }
  fn read_i16(&mut self) -> DecodeResult<i16> { Ok(expect!(self.pop(), Integer, Int16)) }
  fn read_i8(&mut self) -> DecodeResult<i8> { Ok(expect!(self.pop(), Integer, Int8)) }
  fn read_int(&mut self) -> DecodeResult<int> {
    match expect!(self.pop(), Integer) {
      Int64(value) => Ok(value as int),
      Int32(value) => Ok(value as int),
      Int16(value) => Ok(value as int),
      Int8(value) => Ok(value as int),
      value => Err(ExpectedError("Int".to_string(), format!("{}", value)))
    }
  }

  fn read_bool(&mut self) -> DecodeResult<bool> { Ok(expect!(self.pop(), Boolean)) }

  fn read_f64(&mut self) -> DecodeResult<f64> { Ok(expect!(self.pop(), Float, Float64)) }
  fn read_f32(&mut self) -> DecodeResult<f32> { Ok(expect!(self.pop(), Float, Float32)) }

  fn read_char(&mut self) -> DecodeResult<char> { Err(NotSupportedError) }

  fn read_str(&mut self) -> DecodeResult<String> { Ok(expect!(self.pop(), String)) }

  fn read_enum<T>(&mut self, _: &str, _: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> { Err(NotSupportedError) }
  fn read_enum_variant<T>(&mut self, _: &[&str], _: |&mut Decoder, uint| -> DecodeResult<T>) -> DecodeResult<T> { Err(NotSupportedError) }
  fn read_enum_variant_arg<T>(&mut self, _: uint, _: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> { Err(NotSupportedError) }
  fn read_enum_struct_variant<T>(&mut self, _: &[&str], _: |&mut Decoder, uint| -> DecodeResult<T>) -> DecodeResult<T> { Err(NotSupportedError) }
  fn read_enum_struct_variant_field<T>(&mut self, _: &str, _: uint, _: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> { Err(NotSupportedError) }

  fn read_struct<T>(&mut self, _: &str, _: uint, f: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> {
    let value = try!(f(self));
    self.pop();
    Ok(value)
  }
  fn read_struct_field<T>(&mut self, name: &str, _: uint, f: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> {
    let mut obj = expect!(self.pop(), Map);

    let value = match obj.pop(&name.to_string()) {
      None => return Err(MissingFieldError(name.to_string())),
      Some(msgpack) => {
        self.stack.push(msgpack);
        try!(f(self))
      }
    };
    self.stack.push(Map(obj));
    Ok(value)
  }

  fn read_tuple<T>(&mut self, f: |&mut Decoder, uint| -> DecodeResult<T>) -> DecodeResult<T> { self.read_seq(f) }
  fn read_tuple_arg<T>(&mut self, idx: uint, f: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> { self.read_seq_elt(idx, f) }
  fn read_tuple_struct<T>(&mut self, _: &str, f: |&mut Decoder, uint| -> DecodeResult<T>) -> DecodeResult<T> { self.read_tuple(f) }
  fn read_tuple_struct_arg<T>(&mut self, idx: uint, f: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> { self.read_tuple_arg(idx, f) }

  fn read_option<T>(&mut self, _: |&mut Decoder, bool| -> DecodeResult<T>) -> DecodeResult<T> { Err(NotSupportedError) }

  fn read_seq<T>(&mut self, f: |&mut Decoder, uint| -> DecodeResult<T>) -> DecodeResult<T> {
    let list = expect!(self.pop(), Array);
    let len = list.len();
    for v in list.move_iter().rev() {
      self.stack.push(v);
    }
    f(self, len)
  }
  fn read_seq_elt<T>(&mut self, _: uint, f: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> { f(self) }

  fn read_map<T>(&mut self, f: |&mut Decoder, uint| -> DecodeResult<T>) -> DecodeResult<T> {
    let obj = expect!(self.pop(), Map);
    let len = obj.len();
    for (key, value) in obj.move_iter() {
      self.stack.push(value);
      self.stack.push(String(key));
    }
    f(self, len)
  }
  fn read_map_elt_key<T>(&mut self, _: uint, f: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> { f(self) }
  fn read_map_elt_val<T>(&mut self, _: uint, f: |&mut Decoder| -> DecodeResult<T>) -> DecodeResult<T> { f(self) }
}

#[cfg(test)]
mod tests {
  extern crate test;

  #[deriving(PartialEq, Encodable, Decodable, Show)]
  struct Inner {
    a: (),
    b: uint,
    c: Vec<String>
  }

  #[test]
  fn test_decode_struct() {
    let b = b"\x83\xA1a\xC0\xA1b\x02\xA1c\x92\xA3abc\xA3xyz";
    let v: Inner = super::decode(b).unwrap();
    assert_eq!(
      v,
      Inner { a: (), b: 2, c: vec!["abc".to_string(), "xyz".to_string()] }
    );
  }
}
