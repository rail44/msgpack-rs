use std::io::{
  IoResult,
  IoError,
  Writer,
  MemWriter
};
use std::mem::{
  transmute,
};
use serialize::Encodable;
use serialize;
use {
  MsgPack,
  Nil,
  Boolean,
  String,
  Integer,
  Int8,
  Int16,
  Int32,
  Int64,
  Uint8,
  Uint16,
  Uint32,
  Uint64,
  Float,
  Float32,
  Float64,
  Array,
};

pub fn encode<'a, T: Encodable<Encoder<'a>, IoError>>(object: &T) -> Vec<u8> {
  Encoder::encode(object)
}

pub struct Encoder<'a> {
  writer: &'a mut Writer,
}

impl<'a> Encoder<'a> {
  pub fn new(writer: &'a mut Writer) -> Encoder<'a> {
    Encoder { writer: writer }
  }

  pub fn encode<T:Encodable<Encoder<'a>, IoError>>(object: &T) -> Vec<u8> {
    let mut m = MemWriter::new();
    unsafe {
      let mut encoder = Encoder::new(&mut m as &mut Writer);
      let _ = object.encode(transmute(&mut encoder));
    }
    m.unwrap()
  }
}

pub type EncodeResult = IoResult<()>;

macro_rules! map_type_byte(
  (FixMap, $byte: expr) => (0x80 + $byte);
  (FixArray, $byte: expr) => (0x90 + $byte);
  (FixString, $byte: expr) => (0xa0 + $byte);
  (Nil) => (0xc0);
  (False) => (0xc2);
  (True) => (0xc3);
  (Float32) => (0xca);
  (Float64) => (0xcb);
  (Uint8) => (0xcc);
  (Uint16) => (0xcd);
  (Uint32) => (0xce);
  (Uint64) => (0xcf);
  (Int8) => (0xd0);
  (Int16) => (0xd1);
  (Int32) => (0xd2);
  (Int64) => (0xd3);
  (String8) => (0xd9);
  (String16) => (0xda);
  (String32) => (0xdb);
  (Array16) => (0xdc);
  (Array32) => (0xdd);
  (Map16) => (0xde);
  (Map32) => (0xdf);
)

macro_rules! write_data(
  (Uint64, $v: expr) => (self.writer.write_be_u64($v));
  (Uint32, $v: expr) => (self.writer.write_be_u32($v));
  (Uint16, $v: expr) => (self.writer.write_be_u16($v));
  (Uint8, $v: expr) => (self.writer.write_u8($v));

  (Int64, $v: expr) => (self.writer.write_be_i64($v));
  (Int32, $v: expr) => (self.writer.write_be_i32($v));
  (Int16, $v: expr) => (self.writer.write_be_i16($v));
  (Int8, $v: expr) => (self.writer.write_i8($v));

  (Float64, $v: expr) => (self.writer.write_be_f64($v));
  (Float32, $v: expr) => (self.writer.write_be_f32($v));

  (String, $v: expr) => (self.writer.write_str($v));
  (FixString, $v: expr) => (write_data!(String, $v));
  (String8, $v: expr) => (write_data!(String, $v));
  (String16, $v: expr) => (write_data!(String, $v));
  (String32, $v: expr) => (write_data!(String, $v));

  (Container, $f: expr) => ($f(self));
  (FixMap, $f: expr) => (write_data!(Container, $f));
  (Map16, $f: expr) => (write_data!(Container, $f));
  (Map32, $f: expr) => (write_data!(Container, $f));
  (FixArray, $f: expr) => (write_data!(Container, $f));
  (Array16, $f: expr) => (write_data!(Container, $f));
  (Array32, $f: expr) => (write_data!(Container, $f));
)

macro_rules! write_value(
  ($t: ident) => ( self.writer.write_u8(map_type_byte!($t) as u8) );
  ($t: ident, $v: expr) => ({
    try!(self.writer.write_u8(map_type_byte!($t) as u8));
    write_data!($t, $v)
  });
  ($t: ident, $byte: expr, $v: expr) => ({
    try!(self.writer.write_u8(map_type_byte!($t, $byte) as u8));
    write_data!($t, $v)
  });
)

impl<'a> serialize::Encoder<IoError> for Encoder<'a> {
  fn emit_nil(&mut self) -> EncodeResult { write_value!(Nil) }

  // FIXME: Choose type have minimum sizes.
  fn emit_uint(&mut self, v: uint) -> EncodeResult { self.emit_u64(v as u64) }
  fn emit_u64(&mut self, v: u64) -> EncodeResult { write_value!(Uint64, v) }
  fn emit_u32(&mut self, v: u32) -> EncodeResult { write_value!(Uint32, v) }
  fn emit_u16(&mut self, v: u16) -> EncodeResult { write_value!(Uint16, v) }
  fn emit_u8(&mut self, v: u8) -> EncodeResult { write_value!(Uint8, v) }

  fn emit_int(&mut self, v: int) -> EncodeResult { self.emit_i64(v as i64) }
  fn emit_i64(&mut self, v: i64) -> EncodeResult { write_value!(Int64, v) }
  fn emit_i32(&mut self, v: i32) -> EncodeResult { write_value!(Int32, v) }
  fn emit_i16(&mut self, v: i16) -> EncodeResult { write_value!(Int16, v) }
  fn emit_i8(&mut self, v: i8) -> EncodeResult { write_value!(Int8, v) }

  fn emit_bool(&mut self, v: bool) -> EncodeResult {
    if v {
      write_value!(True)
    } else {
      write_value!(False)
    }
  }

  fn emit_f64(&mut self, v: f64) -> EncodeResult { write_value!(Float64, v) }
  fn emit_f32(&mut self, v: f32) -> EncodeResult { write_value!(Float32, v) }

  fn emit_char(&mut self, _: char) -> EncodeResult { Err(IoError::last_error()) }

  fn emit_str(&mut self, v: &str) -> EncodeResult {
    match v.len() {
      0 .. 31 => write_value!(FixString, v.len(), v),
      32 .. 255 => write_value!(String8, v),
      256 .. 65535 => write_value!(String16, v),
      65536 .. 4294967295 => write_value!(String32, v),
      _ => Err(IoError::last_error())
    }
  }

  fn emit_enum(&mut self, _name: &str, _: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { Err(IoError::last_error()) }
  fn emit_enum_variant(&mut self, _: &str, _: uint, _: uint, _: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { Err(IoError::last_error()) }
  fn emit_enum_variant_arg(&mut self, _: uint, _: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { Err(IoError::last_error()) }
  fn emit_enum_struct_variant(&mut self, name: &str, id: uint, cnt: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { self.emit_enum_variant(name, id, cnt, f) }
  fn emit_enum_struct_variant_field(&mut self, _: &str, idx: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { self.emit_enum_variant_arg(idx, f) }

  fn emit_struct(&mut self, _: &str, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    match len {
      0 .. 15 => write_value!(FixMap, len, f),
      16 .. 65535 => write_value!(Map16, f),
      65536 .. 4294967295 => write_value!(Map32, f),
      _ => Err(IoError::last_error())
    }
  }
  fn emit_struct_field(&mut self, name: &str, _: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    try!(self.emit_str(name));
    f(self)
  }

  fn emit_tuple(&mut self, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { self.emit_seq(len, f) }
  fn emit_tuple_arg(&mut self, idx: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { self.emit_seq_elt(idx, f) }
  fn emit_tuple_struct(&mut self, _: &str, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { self.emit_seq(len, f) }
  fn emit_tuple_struct_arg(&mut self, idx: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { self.emit_seq_elt(idx, f) }

  fn emit_option(&mut self, _: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { Err(IoError::last_error()) }
  fn emit_option_none(&mut self) -> EncodeResult { Err(IoError::last_error()) }
  fn emit_option_some(&mut self, _: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { Err(IoError::last_error()) }

  fn emit_seq(&mut self, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    match len {
      0 .. 15 => write_value!(FixArray, len, f),
      16 .. 65535 => write_value!(Array16, f),
      65536 .. 4294967295 => write_value!(Array32, f),
      _ => Err(IoError::last_error())
    }
  }
  fn emit_seq_elt(&mut self, _: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { f(self) }

  fn emit_map(&mut self, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    match len {
      0 .. 15 => write_value!(FixMap, len, f),
      16 .. 65535 => write_value!(Map16, f),
      65536 .. 4294967295 => write_value!(Map32, f),
      _ => Err(IoError::last_error())
    }
  }
  fn emit_map_elt_key(&mut self, _: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { f(self) }
  fn emit_map_elt_val(&mut self, _: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { f(self) }
}

pub trait ToMsgPack {
  fn to_msgpack(&self) -> MsgPack;
}

macro_rules! to_msgpack_values(
  ($($t: ty (&$me: ident) $block: block)+) => (
    $(
      impl ToMsgPack for $t {
        fn to_msgpack(&$me) -> MsgPack $block
      }
    )+
  )
)

to_msgpack_values!(
  MsgPack(&self) { self.clone() }

  i8(&self) { Integer(Int8(*self)) }
  i16(&self) { Integer(Int16(*self)) }
  i32(&self) { Integer(Int32(*self)) }
  i64(&self) { Integer(Int64(*self)) }

  u8(&self) { Integer(Uint8(*self)) }
  u16(&self) { Integer(Uint16(*self)) }
  u32(&self) { Integer(Uint32(*self)) }
  u64(&self) { Integer(Uint64(*self)) }

  f32(&self) { Float(Float32(*self)) }
  f64(&self) { Float(Float64(*self)) }

  String(&self) { String(self.clone()) }
  ()(&self) { Nil }
  bool(&self) { Boolean(*self) }
)

macro_rules! to_msgpack_tuple {
  ($($tyvar:ident),*) => {
    impl<$($tyvar: ToMsgPack),*> ToMsgPack for ($($tyvar),*,) {
      #[inline]
      #[allow(uppercase_variables)]
      fn to_msgpack(&self) -> MsgPack {
        match *self {
          ($(ref $tyvar),*,) => Array(vec![$($tyvar.to_msgpack()),*])
        }
      }
    }
  }
}

to_msgpack_tuple!(A)
to_msgpack_tuple!(A, B)
to_msgpack_tuple!(A, B, C)
to_msgpack_tuple!(A, B, C, D)
to_msgpack_tuple!(A, B, C, D, E)
to_msgpack_tuple!(A, B, C, D, E, F)
to_msgpack_tuple!(A, B, C, D, E, F, G)
to_msgpack_tuple!(A, B, C, D, E, F, G, H)
to_msgpack_tuple!(A, B, C, D, E, F, G, H, I)
to_msgpack_tuple!(A, B, C, D, E, F, G, H, I, J)
to_msgpack_tuple!(A, B, C, D, E, F, G, H, I, J, K)
to_msgpack_tuple!(A, B, C, D, E, F, G, H, I, J, K, L)

#[cfg(test)]
mod test {
  #[deriving(PartialEq, Encodable, Decodable, Show)]
  struct Inner {
    a: (),
    b: u8,
    c: Vec<String>
  }

  #[test]
  fn test_encode_for_struct() {
    let obj = Inner { a: (), b: 2, c: vec!["abc".to_string(), "xyz".to_string()] };
    let b = b"\x83\xA1a\xC0\xA1b\xcc\x02\xA1c\x92\xA3abc\xA3xyz";
    assert_eq!(
      super::encode(&obj).as_slice(),
      b
    );
  }
}
