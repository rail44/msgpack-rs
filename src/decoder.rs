use serialize;
use serialize::Decodable;
use std::string::String as RustString;

use MsgPack;
use MsgPack::{
    Nil,
    Boolean,
    String,
    Integer,
    Float,
    Array,
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
use self::DecoderError::{ApplicationError, ExpectedError, MissingFieldError, NotSupportedError};
use self::DecoderMode::{Strict, Soft};

macro_rules! expect(
    ($e:expr, Nil) => {
        try!(
            match $e {
                Some(v) => match v {
                    Nil => Ok(()),
                    other => Err(ExpectedError("Null".to_string(), format!("{}", other)))
                },
                None => Err(ExpectedError("Null".to_string(), "EOF".into_string()))
            }
        )
    };
    ($e:expr, $t:ident) => {
        try!(
            match $e {
                Some(v1) => match v1 {
                    $t(v) => Ok(v),
                    other => Err(ExpectedError(stringify!($t).to_string(), format!("{}", other)))
                },
                None => Err(ExpectedError(stringify!($t).to_string(), "EOF".into_string()))
            }
        )
    };
    ($e:expr, $t:ident, $($t_rest:ident),+) => {
        expect!(Some(*expect!($e, $t)), $($t_rest),+)
    }
)

#[deriving(Clone, PartialEq, Show)]
pub enum DecoderError {
    NotSupportedError,
    ExpectedError(RustString, RustString),
    MissingFieldError(RustString),
    ApplicationError(RustString)
}

pub enum DecoderMode {
    Strict,
    Soft
}

impl Copy for DecoderMode {}

// Decoder
pub struct Decoder {
    stack: Vec<MsgPack>,
    mode: DecoderMode
}

type DecodeResult<T> = Result<T, DecoderError>;

impl Decoder {
    pub fn new(msgpack: MsgPack, mode: DecoderMode) -> Decoder {
        Decoder { stack: vec![msgpack], mode: mode }
    }

    fn pop(&mut self) -> Option<MsgPack> {
        self.stack.pop()
    }
}


pub fn decode<T: Decodable<Decoder, DecoderError>>(s: &[u8], mode: DecoderMode) -> Result<T, DecoderError> {
    let msgpack = match MsgPack::from_bytes(s) {
        Ok(x) => x,
        Err(e) => panic!("{}",e)
    };

    let mut decoder = Decoder::new(msgpack, mode);
    Decodable::decode(&mut decoder)
}

impl serialize::Decoder<DecoderError> for Decoder {
    fn read_nil(&mut self) -> DecodeResult<()> { Ok(expect!(self.pop(), Nil)) }

    fn read_u64(&mut self) -> DecodeResult<u64> { Ok(expect!(self.pop(), Integer, Uint64)) }
    fn read_u32(&mut self) -> DecodeResult<u32> { Ok(expect!(self.pop(), Integer, Uint32)) }
    fn read_u16(&mut self) -> DecodeResult<u16> { Ok(expect!(self.pop(), Integer, Uint16)) }
    fn read_u8(&mut self) -> DecodeResult<u8> { Ok(expect!(self.pop(), Integer, Uint8)) }
    fn read_uint(&mut self) -> DecodeResult<uint> {
        match *expect!(self.pop(), Integer) {
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
        match *expect!(self.pop(), Integer) {
            Int64(value) => Ok(value as int),
            Int32(value) => Ok(value as int),
            Int16(value) => Ok(value as int),
            Int8(value) => Ok(value as int),
            value => Err(ExpectedError("Int".to_string(), format!("{}", value)))
        }
    }

    fn read_bool(&mut self) -> DecodeResult<bool> { Ok(*expect!(self.pop(), Boolean)) }

    fn read_f64(&mut self) -> DecodeResult<f64> { Ok(expect!(self.pop(), Float, Float64)) }
    fn read_f32(&mut self) -> DecodeResult<f32> { Ok(expect!(self.pop(), Float, Float32)) }

    fn read_char(&mut self) -> DecodeResult<char> { Err(NotSupportedError) }

    fn read_str(&mut self) -> DecodeResult<RustString> { Ok(*expect!(self.pop(), String)) }

    fn read_enum<T, F>(&mut self, _: &str, _: F) ->  DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> { Err(NotSupportedError) }

    fn read_enum_variant<T, F>(&mut self, _: &[&str], _: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder, uint) -> DecodeResult<T> { Err(NotSupportedError) }

    fn read_enum_variant_arg<T, F>(&mut self, _: uint, _: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> { Err(NotSupportedError) }

    fn read_enum_struct_variant<T, F>(&mut self, _: &[&str], _: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder, uint) -> DecodeResult<T> { Err(NotSupportedError) }

    fn read_enum_struct_variant_field<T, F>(&mut self, _: &str, _: uint, _: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> { Err(NotSupportedError) }

    fn read_struct<T, F>(&mut self, _: &str, _: uint, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> {
        let value = try!(f(self));
        self.pop();
        Ok(value)
    }

    fn read_struct_field<T, F>(&mut self, name: &str, _: uint, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> {
        let mut obj = expect!(self.pop(), Map);

        match obj.remove(&name.to_string()) {
            None => match self.mode {
                Strict => return Err(MissingFieldError(name.to_string())),
                Soft => self.stack.push(Nil)
            },
            Some(msgpack) => self.stack.push(msgpack)
        };
        let value = try!(f(self));
        self.stack.push(Map(obj));
        Ok(value)
    }

    fn read_tuple<T, F>(&mut self, _: uint, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> {
        self.read_seq(move |d: &mut Decoder, _: uint| -> DecodeResult<T> { f(d) })
    }

    fn read_tuple_arg<T, F>(&mut self, idx: uint, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> { self.read_seq_elt(idx, f) }

    fn read_tuple_struct<T, F>(&mut self, _: &str, len: uint, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> { self.read_tuple(len, f) }

    fn read_tuple_struct_arg<T, F>(&mut self, idx: uint, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> { self.read_tuple_arg(idx, f) }

    fn read_option<T, F>(&mut self, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder, bool) -> DecodeResult<T> {
        match self.pop() {
            Some(v) => match v {
                Nil => f(self, false),
                value => {
                    self.stack.push(value);
                    f(self, true)
                }
            },
            None => Err(ExpectedError(stringify!($t).to_string(), "EOF".into_string()))
        }
    }

    fn read_seq<T, F>(&mut self, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder, uint) -> DecodeResult<T> {
        let list = expect!(self.pop(), Array);
        let len = list.len();
        for v in list.into_iter().rev() {
            self.stack.push(v);
        }
        f(self, len)
    }

    fn read_seq_elt<T, F>(&mut self, _: uint, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> { f(self) }

    fn read_map<T, F>(&mut self, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder, uint) -> DecodeResult<T> {
        let obj = expect!(self.pop(), Map);
        let len = obj.len();
        for (key, value) in obj.into_iter() {
            self.stack.push(value);
            self.stack.push(String(box key));
        }
        f(self, len)
    }

    fn read_map_elt_key<T, F>(&mut self, _: uint, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> { f(self) }

    fn read_map_elt_val<T, F>(&mut self, _: uint, f: F) -> DecodeResult<T>
    where F: FnOnce(&mut Decoder) -> DecodeResult<T> { f(self) }

    fn error(&mut self, err: &str) -> DecoderError { ApplicationError(err.to_string()) }
}

#[cfg(test)]
mod test {
    #[deriving(PartialEq, Encodable, Decodable, Show)]
    struct Inner {
        a: (),
        b: uint,
        c: Vec<String>
    }

    #[test]
    fn test_decode_for_struct() {
        let b = b"\x83\xA1a\xC0\xA1b\x02\xA1c\x92\xA3abc\xA3xyz";
        let v: Inner = super::decode(b, super::DecoderMode::Strict).unwrap();
        assert_eq!(
            v,
            Inner { a: (), b: 2, c: vec!["abc".to_string(), "xyz".to_string()] }
        );
    }
}
