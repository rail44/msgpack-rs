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
impl<'a> serialize::Encoder<IoError> for Encoder<'a> {
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
