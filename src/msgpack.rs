#![feature(phase, macro_rules)]

#[phase(plugin, link)]
extern crate log;

extern crate serialize;
use std::collections::BTreeMap;
use serialize::{
    Decodable,
    Encodable
};
use std::io::{
    IoResult,
    BufReader
};
use std::string::String as RustString;
use MsgPack::{
    Nil,
    Boolean,
    String,
    Integer,
    Float,
    Array,
    Binary,
    Extended,
    Map
};
use IntegerValue::{
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64
};
use FloatValue::{
    Float32,
    Float64
};

pub use parser::{
    Parser,
    StreamParser
};
pub use encoder::{
    encode,
    Encoder,
    ToMsgPack
};
pub use decoder::{
    decode,
    Decoder,
};
pub mod parser;
pub mod decoder;
pub mod encoder;

#[deriving(Clone, PartialEq, Show)]
pub enum MsgPack {
    Integer(Box<IntegerValue>),
    Nil,
    Boolean(Box<bool>),
    Float(Box<FloatValue>),
    String(Box<RustString>),
    Binary(Box<Vec<u8>>),
    Array(Box<Vec<MsgPack>>),
    Map(Box<BTreeMap<RustString, MsgPack>>),
    Extended(Box<(i8, Vec<u8>)>)
}

impl<E: serialize::Encoder<S>, S> Encodable<E, S> for MsgPack {
    fn encode(&self, e: &mut E) -> Result<(), S> {
        match self {
            &Integer(ref v) => v.encode(e),
            &Nil => e.emit_nil(),
            &Boolean(ref v) => v.encode(e),
            &String(ref v) => v.encode(e),
            &Binary(ref v) => v.encode(e),
            &Array(ref v) => v.encode(e),
            &Map(ref v) => v.encode(e),
            &Float(ref v) => v.encode(e),
            &Extended(ref v) => v.encode(e),
        }
    }
}

impl IntoBytes for MsgPack {
    fn into_bytes(self) -> Vec<u8> {
        encode(&self)
    }
}

impl MsgPack {
    pub fn to_writer(&self, writer: &mut Writer) -> IoResult<()> {
        let mut encoder = Encoder::new(writer);
        self.encode(&mut encoder)
    }

    pub fn from_bytes(b: &[u8]) -> IoResult<MsgPack> {
        let reader = BufReader::new(b);
        let mut parser = Parser::new(reader);
        parser.parse()
    }

    pub fn decode<T: Decodable<Decoder, decoder::DecoderError>>(self, mode: decoder::DecoderMode) -> Result<T, decoder::DecoderError> {
        let mut decoder = Decoder::new(self, mode);
        Decodable::decode(&mut decoder)
    }

    pub fn find<'a>(&'a self, key: &RustString) -> Option<&'a MsgPack>{
        match self {
            &Map(ref map) => map.get(key),
            _ => None
        }
    }

    pub fn contains_key<'a>(&'a self, key: &RustString) -> bool {
        match self {
            &Map(ref map) => map.contains_key(key),
            _ => false
        }
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

impl Copy for IntegerValue {}

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

impl Copy for FloatValue {}

impl<E: serialize::Encoder<S>, S> Encodable<E, S> for FloatValue {
    fn encode(&self, e: &mut E) -> Result<(), S> {
        match *self {
            Float32(v) => v.encode(e),
            Float64(v) => v.encode(e)
        }
    }
}
