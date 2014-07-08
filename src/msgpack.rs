#![feature(phase, macro_rules)]
#![crate_id = "msgpack#0.0.1"]
#![crate_type = "lib"]

#[phase(plugin, link)]
extern crate log;

extern crate serialize;
use std::collections::HashMap;
use serialize::{
  Encodable,
  Encoder
};
use encoder::encode;
pub mod parser;
pub mod decoder;
pub mod encoder;

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

impl<E: serialize::Encoder<S>, S> Encodable<E, S> for MsgPack {
  fn encode(&self, e: &mut E) -> Result<(), S> {
    match *self {
      Integer(v) => v.encode(e),
      Nil => e.emit_nil(),
      Boolean(v) => v.encode(e),
      String(ref v) => v.encode(e),
      Binary(ref v) => v.encode(e),
      Array(ref v) => v.encode(e),
      Map(ref v) => v.encode(e),
      Float(ref v) => v.encode(e),
      Extended(ref v) => v.encode(e),
    }
  }
}

impl IntoBytes for MsgPack {
  fn into_bytes(self) -> Vec<u8> {
    encode(&self)
  }
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

impl<E: serialize::Encoder<S>, S> Encodable<E, S> for IntegerValue {
  fn encode(&self, e: &mut E) -> Result<(), S> {
    match *self {
      Int8(v) => v.encode(e),
      Int16(v) => v.encode(e),
      Int32(v) => v.encode(e),
      Int64(v) => v.encode(e),
      Uint8(v) => v.encode(e),
      Uint16(v) => v.encode(e),
      Uint32(v) => v.encode(e),
      Uint64(v) => v.encode(e)
    }
  }
}

#[deriving(Clone, PartialEq, Show)]
pub enum FloatValue {
  Float32(f32),
  Float64(f64)
}

impl<E: serialize::Encoder<S>, S> Encodable<E, S> for FloatValue {
  fn encode(&self, e: &mut E) -> Result<(), S> {
    match *self {
      Float32(v) => v.encode(e),
      Float64(v) => v.encode(e)
    }
  }
}
