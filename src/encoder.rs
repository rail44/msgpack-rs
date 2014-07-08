use std::io::{
  IoResult,
  IoError,
  Writer,
  MemWriter
};
use std::mem::{transmute};
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
  Map,
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
  (Nil) => ('\xc0');
  (False) => ('\xc2');
  (True) => ('\xc3');
  (Uint64) => ('\xcf');
  (Uint32) => ('\xce');
  (Uint16) => ('\xcd');
  (Uint8) => ('\xcc');
  (Int64) => ('\xd3');
  (Int32) => ('\xd2');
  (Int16) => ('\xd1');
  (Int8) => ('\xd0');
  (Float64) => ('\xcb');
  (Float32) => ('\xca');
  (String8) => ('\xd9');
  (String16) => ('\xda');
  (String32) => ('\xdb');
)

macro_rules! write_for_type(
  (Uint64) => (self.writer.write_be_u64);
  (Uint32) => (self.writer.write_be_u32);
  (Uint16) => (self.writer.write_be_u16);
  (Uint8) => (self.writer.write_u8);
  (Int64) => (self.writer.write_be_i64);
  (Int32) => (self.writer.write_be_i32);
  (Int16) => (self.writer.write_be_i16);
  (Int8) => (self.writer.write_be_i8);
  (Float64) => (self.writer.write_be_f64);
  (Float32) => (self.writer.write_be_f32);
  (String8) => (self.writer.write_str);
  (String16) => (self.writer.write_str);
  (String32) => (self.writer.write_str);
)

macro_rules! write_value(
  ($t: ident) => ( self.writer.write_chr(map_type_byte!($t)) );
  (String, $v: expr) => ({
    match $v.byte_capacity() {
      0..255 => write_value!(String8, $v),
      256..65535 => write_value!(String16, $v),
      65536..4294967295 => write_value!(String32, $v)
    }
  });
  ($t: ident, $v: expr) => ({
    try!(write_value!($t));
    write_for_type!($t)($v);
  })
)

impl<'a> serialize::Encoder<IoError> for Encoder<'a> {
  fn emit_nil(&mut self) -> EncodeResult { write_value!(Nil) }

  fn emit_uint(&mut self, v: uint) -> EncodeResult { self.emit_u64(v) }
  fn emit_u64(&mut self, v: u64) -> EncodeResult { write_value!(Uint64, v) }
  fn emit_u32(&mut self, v: u32) -> EncodeResult { write_value!(Uint32, v) }
  fn emit_u16(&mut self, v: u16) -> EncodeResult { write_value!(Uint16, v) }
  fn emit_u8(&mut self, v: u8) -> EncodeResult { write_value!(Uint8, v) }

  fn emit_int(&mut self, v: int) -> EncodeResult { self.emit_i64(v) }
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

  fn emit_char(&mut self, v: char) -> EncodeResult { Err(NotSupportedError) }

  fn emit_str(&mut self, v: &str) -> EncodeResult { write_value!(String, v) }

  fn emit_enum(&mut self, _name: &str, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { Err(NotSupportedError) }
  fn emit_enum_variant(&mut self, name: &str, _: uint, cnt: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { Err(NotSupportedError) }
  fn emit_enum_variant_arg(&mut self, idx: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { Err(NotSupportedError) }

  fn emit_enum_struct_variant(&mut self, name: &str, id: uint, cnt: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { self.emit_enum_variant(name, id, cnt, f) }
  fn emit_enum_struct_variant_field(&mut self, _: &str, idx: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { self.emit_enum_variant_arg(idx, f) }
  fn emit_struct(&mut self, _: &str, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    if len == 0 {
      write!(self.writer, "{{}}")
    } else {
      try!(write!(self.writer, "{{"));
      self.indent += 2;
      try!(f(self));
      self.indent -= 2;
      try!(write!(self.writer, "\n"));
      try!(spaces(self.writer, self.indent));
      write!(self.writer, "}}")
    }
  }

  fn emit_struct_field(&mut self,
                       name: &str,
                       idx: uint,
                       f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    if idx == 0 {
      try!(write!(self.writer, "\n"));
    } else {
      try!(write!(self.writer, ",\n"));
    }
    try!(spaces(self.writer, self.indent));
    try!(escape_str(self.writer, name));
    try!(write!(self.writer, ": "));
    f(self)
  }

  fn emit_tuple(&mut self,
                len: uint,
                f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    self.emit_seq(len, f)
  }
  fn emit_tuple_arg(&mut self,
                    idx: uint,
                    f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    self.emit_seq_elt(idx, f)
  }

  fn emit_tuple_struct(&mut self,
                       _: &str,
                       len: uint,
                       f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    self.emit_seq(len, f)
  }
  fn emit_tuple_struct_arg(&mut self,
                           idx: uint,
                           f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    self.emit_seq_elt(idx, f)
  }

  fn emit_option(&mut self, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    f(self)
  }
  fn emit_option_none(&mut self) -> EncodeResult { self.emit_nil() }
  fn emit_option_some(&mut self, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    f(self)
  }

  fn emit_seq(&mut self,
              len: uint,
              f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    if len == 0 {
      write!(self.writer, "[]")
    } else {
      try!(write!(self.writer, "["));
      self.indent += 2;
      try!(f(self));
      self.indent -= 2;
      try!(write!(self.writer, "\n"));
      try!(spaces(self.writer, self.indent));
      write!(self.writer, "]")
    }
  }

  fn emit_seq_elt(&mut self,
                  idx: uint,
                  f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    if idx == 0 {
      try!(write!(self.writer, "\n"));
    } else {
      try!(write!(self.writer, ",\n"));
    }
    try!(spaces(self.writer, self.indent));
    f(self)
  }

  fn emit_map(&mut self,
              len: uint,
              f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    if len == 0 {
      write!(self.writer, "{{}}")
    } else {
      try!(write!(self.writer, "{{"));
      self.indent += 2;
      try!(f(self));
      self.indent -= 2;
      try!(write!(self.writer, "\n"));
      try!(spaces(self.writer, self.indent));
      write!(self.writer, "}}")
    }
  }

  fn emit_map_elt_key(&mut self, idx: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
    if idx == 0 {
      try!(write!(self.writer, "\n"));
    } else {
      try!(write!(self.writer, ",\n"));
    }
    try!(spaces(self.writer, self.indent));
    let mut buf = MemWriter::new();
    unsafe {
      let mut check_encoder = Encoder::new(&mut buf);
      try!(f(transmute(&mut check_encoder)));
    }
    let out = str::from_utf8_owned(buf.unwrap()).unwrap();
    let out = out.as_slice();
    let needs_wrapping = out.char_at(0) != '"' && out.char_at_reverse(out.len()) != '"';
    if needs_wrapping { try!(write!(self.writer, "\"")); }
    try!(f(self));
    if needs_wrapping { try!(write!(self.writer, "\"")); }
    Ok(())
  }

  fn emit_map_elt_val(&mut self, _idx: uint, f: |&mut Encoder| -> EncodeResult) -> EncodeResult {
    try!(write!(self.writer, ": "));
    f(self)
  }
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

  String(&self) { String(*self) }
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
    b: uint,
    c: Vec<String>
  }

  #[test]
  fn test_decode_for_struct() {
    let obj = Inner { a: (), b: 2, c: vec!["abc".to_string(), "xyz".to_string()] };
    let b = b"\x83\xA1a\xC0\xA1b\x02\xA1c\x92\xA3abc\xA3xyz";
    assert_eq!(
      super::encode(&obj).as_slice(),
      b
    );
  }
}
