use std::io::{
    IoResult,
    IoError,
    Writer,
    MemWriter
};
use std::mem::{
    transmute,
};
use std::collections::TreeMap;
use std::string::String as RustString;
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
    Map
};

pub fn encode<'a, T: Encodable<Encoder<'a>, IoError>>(object: &T) -> Vec<u8> {
    Encoder::encode(object)
}

pub struct Encoder<'a> {
    writer: &'a mut Writer+'a,
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
    (FixMap, $len: expr) => (0x80 + $len);
    (FixArray, $len: expr) => (0x90 + $len);
    (FixString, $len: expr) => (0xa0 + $len);
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
    (String8, $len: expr) => (0xd9);
    (String16, $len: expr) => (0xda);
    (String32, $len: expr) => (0xdb);
    (Array16, $len: expr) => (0xdc);
    (Array32, $len: expr) => (0xdd);
    (Map16, $len: expr) => (0xde);
    (Map32, $len: expr) => (0xdf);
)

macro_rules! write_data(
    ($slf: expr, Uint64, $v: expr) => ($slf.writer.write_be_u64($v));
    ($slf: expr, Uint32, $v: expr) => ($slf.writer.write_be_u32($v));
    ($slf: expr, Uint16, $v: expr) => ($slf.writer.write_be_u16($v));
    ($slf: expr, Uint8, $v: expr) => ($slf.writer.write_u8($v));

    ($slf: expr, Int64, $v: expr) => ($slf.writer.write_be_i64($v));
    ($slf: expr, Int32, $v: expr) => ($slf.writer.write_be_i32($v));
    ($slf: expr, Int16, $v: expr) => ($slf.writer.write_be_i16($v));
    ($slf: expr, Int8, $v: expr) => ($slf.writer.write_i8($v));

    ($slf: expr, Float64, $v: expr) => ($slf.writer.write_be_f64($v));
    ($slf: expr, Float32, $v: expr) => ($slf.writer.write_be_f32($v));

    ($slf: expr, String, $v: expr) => ($slf.writer.write_str($v));
    ($slf: expr, FixString, $v: expr, $len: expr) => (write_data!($slf, String, $v));
    ($slf: expr, String8, $v: expr, $len: expr) => ({
        try!(write_data!($slf, Uint8, $len));
        write_data!($slf, String, $v)
    });
    ($slf: expr, String16, $v: expr, $len: expr) => ({
        try!(write_data!($slf, Uint16, $len));
        write_data!($slf, String, $v)
    });
    ($slf: expr, String32, $v: expr, $len: expr) => ({
        try!(write_data!($slf, Uint32, $len));
        write_data!($slf, String, $v)
    });

    ($slf: expr, Container, $f: expr) => ($f($slf));
    ($slf: expr, FixMap, $f: expr, $len: expr) => (write_data!($slf, Container, $f));
    ($slf: expr, Map16, $f: expr, $len: expr) => ({
        try!(write_data!($slf, Uint16, $len));
        write_data!($slf, Container, $f)
    });
    ($slf: expr, Map32, $f: expr, $len: expr) => ({
        try!(write_data!($slf, Uint32, $len));
        write_data!($slf, Container, $f)
    });

    ($slf: expr, FixArray, $f: expr, $len: expr) => (write_data!($slf, Container, $f));
    ($slf: expr, Array16, $f: expr, $len: expr) => ({
        try!(write_data!($slf, Uint16, $len));
        write_data!($slf, Container, $f)
    });
    ($slf: expr, Array32, $f: expr, $len: expr) => ({
        try!(write_data!($slf, Uint32, $len));
        write_data!($slf, Container, $f)
    });
)

macro_rules! write_value(
    // for static typos (e.g. Nil, True)
    ($slf: expr, $t: ident) => ( $slf.writer.write_u8(map_type_byte!($t) as u8) );

    ($slf: expr, $t: ident, $v: expr) => ({
        try!($slf.writer.write_u8(map_type_byte!($t) as u8));
        write_data!($slf, $t, $v)
    });

    ($slf: expr, $t: ident, $v: expr, $len: expr) => ({
        try!($slf.writer.write_u8(map_type_byte!($t, $len) as u8));
        write_data!($slf, $t, $v, $len)
    });
)

impl<'a> serialize::Encoder<IoError> for Encoder<'a> {
    fn emit_nil(&mut self) -> EncodeResult { write_value!(self, Nil) }

    fn emit_uint(&mut self, v: uint) -> EncodeResult { self.emit_u64(v as u64) }
    fn emit_u64(&mut self, v: u64) -> EncodeResult { write_value!(self, Uint64, v) }
    fn emit_u32(&mut self, v: u32) -> EncodeResult { write_value!(self, Uint32, v) }
    fn emit_u16(&mut self, v: u16) -> EncodeResult { write_value!(self, Uint16, v) }
    fn emit_u8(&mut self, v: u8) -> EncodeResult { write_value!(self, Uint8, v) }

    fn emit_int(&mut self, v: int) -> EncodeResult { self.emit_i64(v as i64) }
    fn emit_i64(&mut self, v: i64) -> EncodeResult { write_value!(self, Int64, v) }
    fn emit_i32(&mut self, v: i32) -> EncodeResult { write_value!(self, Int32, v) }
    fn emit_i16(&mut self, v: i16) -> EncodeResult { write_value!(self, Int16, v) }
    fn emit_i8(&mut self, v: i8) -> EncodeResult { write_value!(self, Int8, v) }

    fn emit_bool(&mut self, v: bool) -> EncodeResult {
        if v {
            write_value!(self, True)
        } else {
            write_value!(self, False)
        }
    }

    fn emit_f64(&mut self, v: f64) -> EncodeResult { write_value!(self, Float64, v) }
    fn emit_f32(&mut self, v: f32) -> EncodeResult { write_value!(self, Float32, v) }

    fn emit_char(&mut self, _: char) -> EncodeResult { Err(IoError::last_error()) }

    fn emit_str(&mut self, v: &str) -> EncodeResult {
        match v.len() {
            0 .. 31 => write_value!(self, FixString, v, v.len()),
            32 .. 255 => write_value!(self, String8, v, v.len() as u8),
            256 .. 65535 => write_value!(self, String16, v, v.len() as u16),
            65536 .. 4294967295 => write_value!(self, String32, v, v.len() as u32),
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
            0 .. 15 => write_value!(self, FixMap, f, len),
            16 .. 65535 => write_value!(self, Map16, f, len as u16),
            65536 .. 4294967295 => write_value!(self, Map32, f, len as u32),
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

    fn emit_option(&mut self, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { f(self) }
    fn emit_option_none(&mut self) -> EncodeResult { write_value!(self, Nil) }
    fn emit_option_some(&mut self, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { f(self) }

    fn emit_seq(&mut self, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        match len {
            0 .. 15 => write_value!(self, FixArray, f, len),
            16 .. 65535 => write_value!(self, Array16, f, len as u16),
            65536 .. 4294967295 => write_value!(self, Array32, f, len as u32),
            _ => Err(IoError::last_error())
        }
    }
    fn emit_seq_elt(&mut self, _: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult { f(self) }

    fn emit_map(&mut self, len: uint, f: |&mut Encoder<'a>| -> EncodeResult) -> EncodeResult {
        match len {
            0 .. 15 => write_value!(self, FixMap, f, len),
            16 .. 65535 => write_value!(self, Map16, f, len as u16),
            65536 .. 4294967295 => write_value!(self, Map32, f, len as u32),
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

    i8(&self) { Integer(box Int8(*self)) }
    i16(&self) { Integer(box Int16(*self)) }
    i32(&self) { Integer(box Int32(*self)) }
    i64(&self) { Integer(box Int64(*self)) }

    u8(&self) { Integer(box Uint8(*self)) }
    u16(&self) { Integer(box Uint16(*self)) }
    u32(&self) { Integer(box Uint32(*self)) }
    u64(&self) { Integer(box Uint64(*self)) }

    f32(&self) { Float(box Float32(*self)) }
    f64(&self) { Float(box Float64(*self)) }

    RustString(&self) { String(box self.clone()) }
    ()(&self) { Nil }
    bool(&self) { Boolean(box *self) }
)

macro_rules! to_msgpack_tuple {
    ($($tyvar:ident),*) => {
        impl<$($tyvar: ToMsgPack),*> ToMsgPack for ($($tyvar),*,) {
            #[inline]
            #[allow(uppercase_variables)]
            fn to_msgpack(&self) -> MsgPack {
                match *self {
                    ($(ref $tyvar),*,) => Array(box vec![$($tyvar.to_msgpack()),*])
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

impl<T: ToMsgPack> ToMsgPack for Vec<T> {
    fn to_msgpack(&self) -> MsgPack { Array(box self.iter().map(|elt| elt.to_msgpack()).collect()) }
}

impl<T: ToMsgPack> ToMsgPack for TreeMap<RustString, T> {
    fn to_msgpack(&self) -> MsgPack {
        let mut d = TreeMap::new();
        for (key, value) in self.iter() {
            d.insert((*key).clone(), value.to_msgpack());
        }
        Map(box d)
    }
}

impl<T: ToMsgPack> ToMsgPack for Option<T> {
    fn to_msgpack(&self) -> MsgPack {
        match *self {
            None => Nil,
            Some(ref value) => value.to_msgpack()
        }
    }
}

impl<'a, T: ToMsgPack+Clone> ToMsgPack for &'a T {
    fn to_msgpack(&self) -> MsgPack {
        self.clone().to_msgpack()
    }
}

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
